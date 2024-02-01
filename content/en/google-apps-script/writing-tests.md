---
title:                "Writing tests"
date:                  2024-02-01T13:42:11.048048-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in Google Apps Script is about constructing checks to validate that your scripts do what you expect them to do. Programmers craft these tests to catch bugs and ensure that future changes don’t break existing functionality.

## How to:

First off, Google Apps Script doesn't have built-in testing frameworks like some other programming environments. But fear not, you can still write simple test functions or leverage external tools like clasp and mocha for more complex testing. Here’s a very basic example:

```Google Apps Script
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Expected 5, but got " + result);
  }
}
```

To run `testAdd`, just select it in the Apps Script editor and press the play button. If nothing happens (no errors), your test passed! If the addition doesn’t work as expected, you'll see an error in the executions log with your message.

For more advanced scenarios, using clasp to download your script projects and run them with mocha might look like this:

1. Download and set up clasp, then log in to your Google account.
2. Clone your project locally using clasp.
3. Set up mocha and any assertion library locally in your project directory.
4. Write your tests in a new local file, say `tests.js`.

```javascript
const assert = require('assert');
const { add } = require('./yourAppsScriptFile');

describe('add', function() {
  it('correctly adds two numbers', function() {
    assert.strictEqual(add(2, 3), 5);
  });
});
```

5. Run your tests with mocha in your terminal.

You'll see output in your terminal indicating whether your tests passed or if there were any errors.

## Deep Dive

Traditionally, testing wasn’t a big part of the Apps Script world, which was more about quick, one-off scripts to automate Google Workspace tasks than about building complex, maintainable systems. However, as Apps Script has grown in power and popularity, the community has found ways to integrate testing into their workflows. 

The native Google Apps Script environment still lacks direct support for testing frameworks, forcing developers to find workarounds or use external tools. While tools like clasp and mocha add extra steps to the process, they also offer a more robust testing environment, potentially saving developers from headaches down the line by catching bugs early.

It’s worth mentioning that if Google Apps Script projects become complex or critical enough, it might be worth evaluating whether Apps Script is still the right tool for the job or if a transition to a more traditional development environment with built-in testing support would be more appropriate. However, for many scenarios, especially those deeply integrated with Google Workspace, the benefits of Apps Script—with its direct access to Google services—often outweigh these testing inconveniences.
