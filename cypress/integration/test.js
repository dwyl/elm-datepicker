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
          .should("contain", dates.month[new Date().getMonth()]);
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
            getOverflowIndex(dates.month, new Date().getMonth() - 1)
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
            getOverflowIndex(dates.month, new Date().getMonth() + 1)
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
      cy
        .get("#previous-month")
        .click()
        .get(`tbody > :nth-child(2) > :nth-child(1)`)
        .click()
        .should("not.have.attr", "aria-selected");
    });

    it("should select valid day", function() {
      getNextDay(todaysPosition())
        .click()
        .invoke("attr", "aria-selected")
        .should("equal", "true");
    });

    it("should deselect last day on new selection", function() {
      cy
        .get("#next-month")
        .click()
        .get(`tbody > :nth-child(2) > :nth-child(1)`)
        .click()
        .invoke("attr", "aria-selected")
        .should("equal", "true");

      cy
        .get(`tbody > :nth-child(2) > :nth-child(2)`)
        .click()
        .invoke("attr", "aria-selected")
        .should("equal", "true");

      cy
        .get(`tbody > :nth-child(2) > :nth-child(1)`)
        .invoke("attr", "aria-selected")
        .should("equal", "false");
    });
  });
});

/* gets position of current day on calendar */
function todaysPosition() {
  const currentMonth = new Date();
  const today = currentMonth.getDate();
  const day = currentMonth.getDay();

  currentMonth.setDate(1);
  const firstOfMonth = currentMonth.getDay();
  const todaysIndex = today + (firstOfMonth === 0 ? 7 : firstOfMonth) - 1;

  return [Math.ceil(todaysIndex / 7), day === 0 ? 7 : day];
}

function getToday() {
  const today = todaysPosition();
  return cy.get(`tbody > :nth-child(${today[0]}) > :nth-child(${today[1]})`);
}

/* Determines position of next day based on current day
  and whether it's the last day of the week or month */
function nextDayPosition(currentDay) {
  if (todayIsLastDayOfMonth()) {
    return [1, (currentDay[1] % 7) + 1]
  }
  const currentDayIsSunday = currentDay[1] === 7;

  return [
    todayIsLastDayOfMonth() ? 1 : (currentDayIsSunday ? currentDay[0] + 1 : currentDay[0]),
    (currentDay[1] % 7) + 1
  ];
}

function getNextDay(currentDay) {
  const nextDay = nextDayPosition(currentDay);

  if (todayIsLastDayOfMonth()) {
    cy
      .get("#next-month")
      .click();
  }

  return cy.get(
    `tbody > :nth-child(${nextDay[0]}) > :nth-child(${nextDay[1]})`
  );
}

function todayIsLastDayOfMonth() {
  const currentDate = new Date();
  const today = currentDate.getDate();

  switch (currentDate.getMonth()) {
    case 1:
      return today === 29 || today === 28;
    case 3:
    case 5:
    case 8:
    case 10:
      return today === 30;
    default:
      return today === 31;
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
