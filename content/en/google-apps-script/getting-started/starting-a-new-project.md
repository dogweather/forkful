---
title:                "Starting a new project"
aliases: - /en/google-apps-script/starting-a-new-project.md
date:                  2024-02-01T21:12:08.207343-07:00
model:                 gpt-4-0125-preview
simple_title:         "Starting a new project"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Google Apps Script (GAS) entails initializing a script file within the Google ecosystem (Google Drive, Docs, Sheets, etc.) to automate tasks or extend Google Apps functionalities. Programmers often embark on this journey to streamline workflows, manipulate Google services programmatically, or create custom add-ons, saving time and harnessing the power of Google's infrastructure.

## How to:

To kick off a new project in Google Apps Script, you have a couple of entry points, but let's focus on the most direct method: creating a script from Google Drive.

1. **Creating a Project in Google Drive**
   - Navigate to Google Drive (drive.google.com).
   - Click "+ New" > "More" > "Google Apps Script".
   - A new script project opens in the editor. By default, it contains a `Code.gs` file with a sample `myFunction`.

2. **Setting Up Your Project**
   - Rename your project for clarity. Click "Untitled project" at the top left, and give it a meaningful name.
   - Write a simple function in the `Code.gs` file to get a feel for it:

```javascript
function helloWorld() {
  Logger.log('Hello, world!');
}
```

   - Run `helloWorld` by selecting the function in the dropdown next to the play button (▶) and clicking it. This will execute the function.

3. **Viewing Logs**
   - To view the output of `Logger.log`, go to "View" > "Logs", or press `Ctrl + Enter`. You should see "Hello, world!" in the logs.

Congratulations, you've just successfully started a new project in Google Apps Script and ran a simple function!

## Deep Dive

The inception of Google Apps Script around 2009 provided a powerful yet approachable platform for both developers and non-developers to automate, extend, and build upon the vast array of Google services. Unlike traditional programming environments, GAS offers a unique blend of simplicity and integration, directly within the Google ecosystem, without the need for external servers or setup. This serverless execution model vastly simplifies project deployment and management.

Historically, GAS was somewhat limited by its execution environment and language version, often lagging behind the current JavaScript standards. However, recent updates have brought modern JavaScript syntax (ECMAScript 2015+) to GAS, making it more palatable for developers accustomed to contemporary development practices.

While GAS is uniquely positioned to interact with Google Services, there are alternative approaches for more intensive or specific needs. For instance, Google Cloud Functions and Google Cloud Platform (GCP) offer more robust and scalable solutions for handling complex workflows, processing large datasets, and integrating with external APIs. These platforms allow for programming in various languages (e.g., Python, Go, Node.js) and offer greater computational resources.

Nonetheless, for tasks intricately tied to Google Apps, automation, and rapid development within this ecosystem, Google Apps Script remains an unmatched tool in terms of ease of use and integration depth. Its accessibility directly from Google Drive and seamless connection to Google services make it a practical choice for a wide array of projects, particularly for those looking to extend the functionality of Sheets, Docs, Forms, and other Google applications.
