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
      getToday()
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
function getToday() {
  const currentMonth = new Date();
  const today = currentMonth.getDate();
  const day = currentMonth.getDay();

  currentMonth.setDate(1);
  const firstOfMonth = currentMonth.getDay();
  const todaysIndex = today + (firstOfMonth === 0 ? 7 : firstOfMonth) - 1;

  const xIndex = Math.ceil(todaysIndex / 7);
  const yIndex = day === 0 ? 7 : day;

  return cy.get(`tbody > :nth-child(${xIndex}) > :nth-child(${yIndex})`);
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
