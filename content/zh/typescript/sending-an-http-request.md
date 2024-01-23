---
title:                "发出 HTTP 请求"
date:                  2024-01-20T18:00:50.027062-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? 什么是HTTP请求以及为什么要用？
发送HTTP请求就是让你的程序通过互联网与服务器交流。程序员这么做以获取资料、发送数据、操作远程服务。

## How to: 怎么做？
在TypeScript中，常用`fetch`函数发送HTTP请求。下面是个例子，我们从一个JSON占位符服务中获取数据。

```TypeScript
// TypeScript 示例：发送 GET 请求来获取数据
async function getData(url: string): Promise<any> {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error('Network response was not ok');
    }
    return await response.json();
  } catch (error) {
    console.error('Fetching data failed:', error);
  }
}

// 使用示例
const url = 'https://jsonplaceholder.typicode.com/todos/1';
getData(url).then(data => console.log(data));

// 输出样例：
// {
//   userId: 1,
//   id: 1,
//   title: 'delectus aut autem',
//   completed: false
// }
```

## Deep Dive 深入探究
早期，发送HTTP请求用的是XMLHttpRequest对象。它复杂，用起来繁琐。后来，Fetch API出现了，它基于Promise，语法更简洁，使用更方便。

现在有几种方式发送HTTP请求：
- Fetch API（现代、简单）
- XMLHttpRequest（老式、复杂）
- 第三方库，如Axios（功能强大、可定制性高）

在具体实现中，可能要处理跨域问题（CORS）、认证和安全性等问题。根据请求的类型（GET、POST、PUT、DELETE等），发送参数和数据处理方式也有所不同。

## See Also 查阅更多
- MDN Web Docs on Fetch API: [Fetch API on MDN](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- JSON Placeholder for testing HTTP requests: [JSON Placeholder](https://jsonplaceholder.typicode.com/)
- Axios GitHub repository for an alternative HTTP client: [Axios on GitHub](https://github.com/axios/axios)
