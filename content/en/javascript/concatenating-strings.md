---
title:    "Javascript recipe: Concatenating strings"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why

Have you ever found yourself wanting to create a single string from two or more smaller strings? This is where concatenating strings comes in handy. By combining strings, you can create a new string that includes all the desired content. In this blog post, we will dive into the world of concatenating strings in Javascript and explore why it is a useful technique for web development.

## How To

Concatenating strings in Javascript is a simple process that involves using the "+" operator to combine two or more strings. Let's take a look at an example:

```Javascript
let firstName = "John";
let lastName = "Smith";
let fullName = firstName + " " + lastName;

console.log(fullName); // Output: John Smith
```

In this example, we have defined two variables `firstName` and `lastName` and assigned them the values "John" and "Smith" respectively. We then use the "+" operator to concatenate these two strings together, adding a space in between to create a full name. By using the `console.log()` function, we can see the output of the new string "John Smith" in the console.

Concatenation is not limited to just two strings, you can combine as many strings as you need. Let's see another example:

```Javascript
let welcome = "Hello ";
let name = "Emily";
let message = welcome + name + "! Welcome to our website.";

console.log(message); // Output: Hello Emily! Welcome to our website.
```

In this example, we have concatenated three strings together to create a personalized message.

It is also possible to concatenate strings with numbers by first converting the numbers to strings using the `toString()` method. Here is an example:

```Javascript
let num1 = 5;
let num2 = 4;
let sum = num1.toString() + num2.toString();

console.log(sum); // Output: 54
```

## Deep Dive

Behind the scenes, when we use the "+" operator to concatenate strings, JavaScript converts them to strings if they are not already. This is known as type coercion. This means that we can also concatenate strings with other data types such as numbers and booleans.

It is important to note that the order of the strings matters when concatenating. In the first example, if we had written `fullName = lastName + " " + firstName;` our output would have been "Smith John". The order in which the strings are combined will determine the final output.

Another thing to keep in mind is the use of spaces when concatenating strings. In the third example, we have to manually add a space after "Hello" to ensure the correct output. Otherwise, our message would have been "HelloEmily! Welcome to our website.".

## See Also

Now that you understand the basics of concatenating strings in Javascript, you may want to explore some other related topics:

- [W3Schools - JavaScript String Concat() Method](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [MDN Web Docs - Working with Strings](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Working_with_Strings)

Happy coding!