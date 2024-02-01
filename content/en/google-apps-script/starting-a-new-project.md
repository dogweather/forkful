---
title:                "Starting a new project"
date:                  2024-02-01T13:42:00.329859-07:00
model:                 gpt-4-0125-preview
simple_title:         "Starting a new project"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Google Apps Script (GAS) is about initializing a fresh script or app within the Google ecosystem, be it for automating Google Sheets, Docs, or building a web app. Programmers do it to tailor Google services to their specific needs, automate repetitive tasks, or create custom functions and interfaces.

## How to:

Getting started on a new project in GAS is straightforward. Here’s a step-by-step guide to kick things off. First, navigate to the Google Apps Script website or open the Script editor from any Google Doc or Sheet by clicking on `Extensions > Apps Script`.

1. **Create a New Project**: On the Google Apps Script dashboard, click on `New Project`. A new script will be created with a default name like `Untitled project`. Rename it by clicking on the project name at the top left corner.

```Google Apps Script
// Rename this to something meaningful
function myFunction() {
  Logger.log('Hello, world!');
}
```
2. **Edit and Save**: You can start coding in the editor. Here’s a simple example that logs a message. Enter the script, hit `Ctrl + S` to save.

3. **Run and Authorize**: The first time you run, you’ll need to authorize the script. Click `Run > Run function > myFunction`, follow the authorization prompts.

4. **Check the Log**: To see your script's output, go to `View > Logs` or press `Ctrl + Enter`.
   
```Google Apps Script
// Output
[DATE-TIME] Hello, world!
```

5. **Beyond**: From here, you can explore adding more functions, using Google Services like Sheets by enabling them under `Services > + Add a service`.

## Deep Dive

Google Apps Script, launched in 2009, is based on JavaScript with additional Google services objects. It’s designed to be easily approachable by beginners but also powerful enough for complex automation and application development within the Google ecosystem. While GAS is incredibly convenient for those already using Google products, it's worth noting that it operates within Google's environment, which can be a limitation for projects requiring a broader or more specialized tech stack. For those projects, considering APIs available for Google services that can be accessed with other programming languages might offer more flexibility. However, for deep integration with Google services and ease of use, particularly for those not looking to set up a complex development environment, GAS remains an invaluable tool.
