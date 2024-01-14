---
title:                "Arduino recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/parsing-html.md"
---

{{< edit_this_page >}}

##Why
Arduino is a popular open-source electronics platform that allows users to create interactive projects. One of the most common uses for Arduino is to build projects that involve web scraping or parsing HTML. By parsing HTML, Arduino can gather information from websites and use that data to trigger actions or display information on connected devices.

##How To
Parsing HTML with Arduino involves a few simple steps. First, you will need to install a library that allows Arduino to read and parse HTML code. One popular library for this is the HTML Parser library by Baldwin Hanno. Once the library is installed, you can start writing your code.

In the code editor, you will first need to include the library by writing ```Arduino
#include <HTMLParser.h>
```

Next, you will need to define the URL of the website you want to scrape. For example, if you want to gather data from the home page of Wikipedia, your code would look like this:

```Arduino
String website = "https://www.wikipedia.org/";
```

After that, you can start parsing the HTML code by using the ```Arduino
parse()``` function from the library. This function will retrieve the HTML code from the website and store it in a variable for further manipulation.

```Arduino
String htmlCode = parse(website);
```

From here, you can start extracting specific data from the HTML code using simple string manipulation functions. For example, if you want to extract the title of the website, you can use the ```Arduino
indexOf()``` and ```Arduino
subString()``` functions to find and isolate the title from the HTML code.

```Arduino
int titleStart = htmlCode.indexOf("<title>");
int titleEnd = htmlCode.indexOf("</title>");
String title = htmlCode.substring(titleStart+7, titleEnd);
```

Once you have extracted the data you need, you can then use that information to control connected devices or display it on a screen. The possibilities are endless with what you can do with this data!

##Deep Dive
Parsing HTML with Arduino may seem daunting, but it can actually be quite simple. The key is understanding the structure of HTML code and how to use string manipulation functions to extract data.

HTML code is made up of tags, which indicate the beginning and end of specific elements on a webpage. These tags are enclosed in angled brackets, such as ```<title>```. By using the ```Arduino toString()``` function, you can convert the HTML code into a string, making it easier to manipulate.

To extract data from the HTML code, you will need to use the ```Arduino
indexOf()``` function. This function searches for a specific string within another string and returns the index of where that string appears. Combined with the ```Arduino
subString()``` function, which allows you to extract a portion of a string, you can isolate specific data from the HTML code.

##See Also
- Arduino HTML Parser library by Baldwin Hanno: https://github.com/balhazra/Arduino-HtmlParser
- Arduino official website: https://www.arduino.cc/
- HTML tutorial for beginners: https://www.w3schools.com/html/
- Project example using Arduino and HTML parsing: https://create.arduino.cc/projecthub/Arnov_Sharma/introduction-to-data-scraping-and-display-on-a-lcd-screen-b4ff8c