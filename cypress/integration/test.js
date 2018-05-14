describe("Testing Example App", function() {
  context("Initialising", function() {
    beforeEach(function() {
      cy.visit("http://localhost:8000/examples/index.html");
      cy.get('[href="#Simple"]').click();
    });

    it(".should() - assert that <title> is correct", function() {
      cy.title().should("include", "DatePicker Example");
    });

    it("should have the current month in the title", function() {
      cy.fixture("dates").then(dates => {
        cy
          .get("#title")
          .should("contain", dates.month[new Date(Date.now()).getMonth()]);
      });
    });

    it("should have the days in the correct format", function() {
      cy.get("thead > tr > :nth-child(1)").should("contain", "Mon");
    });
  });

  context("Changing months", function() {
    beforeEach(function() {
      cy.visit("http://localhost:8000/examples/index.html");
      cy.get('[href="#Simple"]').click();
    });

    it("should go to previous month", function() {
      cy.fixture("dates").then(dates => {
        cy
          .get("#previous-month")
          .click()
          .get("#title")
          .should(
            "contain",
            getOverflowIndex(dates.month, new Date(Date.now()).getMonth() - 1)
          );
      });
    });

    it("should go to next month", function() {
      cy.fixture("dates").then(dates => {
        cy
          .get("#next-month")
          .click()
          .get("#title")
          .should(
            "contain",
            getOverflowIndex(dates.month, new Date(Date.now()).getMonth() + 1)
          );
      });
    });
  });

  context("Selecting dates", function() {
    beforeEach(function() {
      cy.visit("http://localhost:8000/examples/index.html");
      cy.get('[href="#Simple"]').click();
    });

    it("should not select invalid day", function() {
      getToday()
        .click()
        .should("not.have.class", "selected");
    });

    it("should select valid day", function() {
      getNextDay(todaysPosition())
        .click()
        .should("have.class", "selected");
    });

    it("should deselect last day on new selection", function() {
      cy
        .get("#next-month")
        .click()
        .get(`tbody > :nth-child(2) > :nth-child(1)`)
        .click()
        .should("have.class", "selected");

      cy
        .get(`tbody > :nth-child(2) > :nth-child(2)`)
        .click()
        .should("have.class", "selected");

      cy
        .get(`tbody > :nth-child(2) > :nth-child(1)`)
        .should("not.have.class", "selected");
    });
  });
});

/* gets position of current day on calendar */
function todaysPosition() {
  var currentMonth = new Date(Date.now());
  var today = currentMonth.getDate();

  currentMonth.setDate(1);
  var firstOfMonth = currentMonth.getDay();
  var todaysIndex;

  if (firstOfMonth > 0) {
    todaysIndex = today + (firstOfMonth - 1);
  } else {
    // Sunday gives result of 0, but is in position 7 on our calendar
    todaysIndex = today + (7 - 1);
  }

  return [Math.ceil(todaysIndex / 7), todaysIndex % 7];
}

function getToday() {
  var today = todaysPosition();
  return cy.get(`tbody > :nth-child(${today[0]}) > :nth-child(${today[1]})`);
}

/* Determines position of next day based on current day
  and whether it's the last day of the week or month */
function nextDayPosition(currentDay) {
  return [
    lastDay() ? 1 : currentDay[0],
    currentDay[1] === 7 ? 1 : currentDay[1] + 1
  ];
}

function getNextDay(currentDay) {
  var nextDay = nextDayPosition(currentDay);

  if (lastDay()) {
    return cy
      .get("#next-month")
      .click()
      .get(`tbody > :nth-child(${nextDay[0]}) > :nth-child(${nextDay[1]})`);
  } else {
    return cy.get(
      `tbody > :nth-child(${nextDay[0]}) > :nth-child(${nextDay[1]})`
    );
  }
}

function lastDay() {
  var currentDate = new Date(Date.now());

  switch (currentDate.getMonth()) {
    case 1:
      return currentDate.getDate() === 29 || currentDate.getDate() === 28;
      break;
    case 3:
    case 5:
    case 8:
    case 10:
      return currentDate.getDate() === 30;
      break;
    default:
      return currentDate.getDate() === 31;
  }
}

function getOverflowIndex(array, index) {
  if (index < 0) {
    return array[array.length + index];
  } else if (index >= array.length) {
    return array[index % array.length];
  } else {
    return array[index];
  }
}
