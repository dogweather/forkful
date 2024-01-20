---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project refers to building a new application or program from scratch. Programmers do this to solve specific problems, develop new solutions, or learn new techniques and technologies.

## How to:

To start a new JavaScript project, you first need to create a new directory and initialize it with npm (Node Package Manager). 

```Javascript
// Create new directory
$ mkdir my_new_project
$ cd my_new_project

// Initialize npm
$ npm init -y
```

This would create `package.json` file, effectively kick starting your project. 

Now, you can create an `index.js` file and start coding.

```Javascript
// index.js
console.log("Hello, World!");
```

You can then run this example through NodeJS:

```Javascript
$ node index.js
```

This will print:

```
Hello, World!
```

## Deep Dive

Historically, starting a new JS project was often more complex. The arrival of NodeJS and npm made it significantly easier and more structured. 

As alternatives, you can start a new project using `yarn` (another package manager) or coding platforms/tools like `CodePen`, `JSFiddle`, or `Glitch`.

Implementation-wise, more complexity arises as we add more libraries, services, or custom configuration. Tools like `create-react-app` or `vue cli` are also available for starting more complex projects with boilerplate code.

## See Also

- Node.js Docs: [https://nodejs.org/docs/latest-v14.x/api/](https://nodejs.org/docs/latest-v14.x/api/)
- NPM Docs: [https://docs.npmjs.com/](https://docs.npmjs.com/)
- Yarn Docs: [https://classic.yarnpkg.com/en/docs/](https://classic.yarnpkg.com/en/docs/)
- Create React App: [https://create-react-app.dev/](https://create-react-app.dev/)
- Vue CLI: [https://cli.vuejs.org/](https://cli.vuejs.org/)