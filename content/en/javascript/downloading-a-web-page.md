---
title:                "Javascript recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered how a web browser is able to display a webpage? Behind the scenes, your browser is actually downloading the webpage's HTML, CSS, and JavaScript files from a remote server. In this blog post, we will dive into the world of web programming by learning how to download a web page using JavaScript.

## How To

Before we begin, make sure you have a basic understanding of JavaScript syntax and how to run JavaScript code in your browser. Once you're ready, follow these steps to download a web page using JavaScript:

1. Create an HTML file and name it "index.html". This will serve as our base file for running our JavaScript code.
2. Inside the "body" tags of your HTML file, add a "script" tag with the "src" attribute pointing to your JavaScript file. It should look something like this:
```html
<script src="script.js"></script>
```
3. Now, let's create our JavaScript file and name it "script.js". This is where we will write our code for downloading a web page.
4. First, we need to create a new XMLHttpRequest object. This is a built-in JavaScript object that allows us to make HTTP requests.
```javascript
let xhttp = new XMLHttpRequest();
```
5. Next, we need to specify the URL of the web page we want to download. For this example, we will use the Google homepage.
```javascript
let url = "https://www.google.com";
```
6. Now, we need to open a connection to the URL using the "open" method. We also need to specify the type of request we want to make, which in this case is a GET request.
```javascript
xhttp.open("GET", url);
```
7. We also need to specify what should happen when the request is complete. We can do this by setting the "onreadystatechange" property of our XMLHttpRequest object to a function that will be called when the state changes.
```javascript
xhttp.onreadystatechange = function() {
   if (this.readyState == 4 && this.status == 200) {
       // Code to handle the downloaded webpage goes here
   }
};
```
8. Finally, we can send the request using the "send" method and our JavaScript code is complete.
```javascript
xhttp.send();
```
9. When the webpage is successfully downloaded, we can access its contents using the "responseText" property of the XMLHttpRequest object. We can then use this data for further processing or display it on our webpage.

## Deep Dive

Behind the scenes, the process of downloading a web page using JavaScript involves making an HTTP request, receiving a response from the server, and handling the retrieved data. There are other methods for making HTTP requests in JavaScript, such as using the "fetch" API, but the XMLHttpRequest method is still commonly used.

It's worth noting that when making HTTP requests, we need to consider the concept of cross-domain requests, where the request is made to a different domain than the one hosting our JavaScript file. In these cases, the server hosting the webpage may have security measures in place to prevent code from other domains accessing its data. This can be bypassed using techniques like JSONP or CORS, but it's important to understand the security implications before making cross-domain requests.

## See Also

Now that you know how to download a web page using JavaScript, you can explore other advanced features of the XMLHttpRequest object and learn how to handle errors or make more complex requests. Here are some additional resources to help you continue your learning:

- [MDN web docs - XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [W3Schools - XMLHttpRequest Tutorial](https://www.w3schools.com/js/js_ajax_http.asp)
- [Google Developers - Using XMLHttpRequest](https://developers.google.com/web/updates/2015/03/introduction-to-fetch)