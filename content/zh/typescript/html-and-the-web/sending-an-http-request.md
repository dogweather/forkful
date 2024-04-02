---
date: 2024-01-20 18:00:50.027062-07:00
description: "\u53D1\u9001HTTP\u8BF7\u6C42\u5C31\u662F\u8BA9\u4F60\u7684\u7A0B\u5E8F\
  \u901A\u8FC7\u4E92\u8054\u7F51\u4E0E\u670D\u52A1\u5668\u4EA4\u6D41\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u4EE5\u83B7\u53D6\u8D44\u6599\u3001\u53D1\u9001\u6570\u636E\
  \u3001\u64CD\u4F5C\u8FDC\u7A0B\u670D\u52A1\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.468274-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001HTTP\u8BF7\u6C42\u5C31\u662F\u8BA9\u4F60\u7684\u7A0B\u5E8F\u901A\
  \u8FC7\u4E92\u8054\u7F51\u4E0E\u670D\u52A1\u5668\u4EA4\u6D41\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u4EE5\u83B7\u53D6\u8D44\u6599\u3001\u53D1\u9001\u6570\u636E\u3001\
  \u64CD\u4F5C\u8FDC\u7A0B\u670D\u52A1\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
