---
date: 2024-02-03 19:02:39.218495-07:00
description: "How to: In JavaScript, there isn\u2019t a built-in method to directly\
  \ capitalize strings, but it's straightforward to implement using basic string manipulation\u2026"
lastmod: '2024-03-13T22:45:00.418440-06:00'
model: gpt-4-0125-preview
summary: "In JavaScript, there isn\u2019t a built-in method to directly capitalize\
  \ strings, but it's straightforward to implement using basic string manipulation\
  \ methods."
title: Capitalizing a string
weight: 2
---

## How to:
In JavaScript, there isn’t a built-in method to directly capitalize strings, but it's straightforward to implement using basic string manipulation methods.

### Using Standard JavaScript
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Output: "Hello world"
```

### ES6 Version
With ES6 template literals, the function can be written in a more succinct way:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Output: "Hello ES6"
```

### Using Lodash
Lodash is a popular third-party utility library that offers a wide range of functions to manipulate and work with JavaScript values, including strings. To capitalize a string using Lodash:
```javascript
// First, install lodash if you haven’t: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // Output: "Lodash example"
```
_Notice how Lodash not only capitalizes the first letter but also converts the rest of the string to lower case, which differs slightly from the plain JavaScript implementation._

### Using CSS (For Display Purposes Only)
If the goal is to capitalize text for displaying in the UI, CSS can be used:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- Displays as "Hello css" -->
```
**Note:** This method changes how the text appears on the webpage without altering the string itself in JavaScript.
