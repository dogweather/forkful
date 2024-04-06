---
date: 2024-01-26 00:54:42.799525-07:00
description: "\u5982\u4F55\u5904\u7406\uFF1A JavaScript\u4E2D\u7684\u9519\u8BEF\u5904\
  \u7406\u5DF2\u7ECF\u53D1\u5C55\u3002\u65E9\u5728ES3\u65F6\u4EE3\uFF08\u5927\u7EA6\
  1999\u5E74\uFF09\uFF0C\u6211\u4EEC\u53EA\u6709`try-catch`\u5757\u3002\u867D\u7136\
  \u4E0D\u662F\u975E\u5E38\u7075\u6D3B\uFF0C\u4F46\u5B83\u5B8C\u6210\u4E86\u4EFB\u52A1\
  \u3002 ES6\uFF082015\uFF09\u5F15\u5165\u4E86Promises\uFF0C\u5E76\u63D0\u4F9B\u4E86\
  `.then()`\u548C`.catch()`\uFF0C\u4F7F\u6211\u4EEC\u80FD\u591F\u66F4\u4F18\u96C5\u5730\
  \u5904\u7406\u5F02\u6B65\u9519\u8BEF\u3002"
lastmod: '2024-04-05T22:51:01.420199-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u5904\u7406\uFF1A JavaScript\u4E2D\u7684\u9519\u8BEF\u5904\u7406\
  \u5DF2\u7ECF\u53D1\u5C55\u3002\u65E9\u5728ES3\u65F6\u4EE3\uFF08\u5927\u7EA61999\u5E74\
  \uFF09\uFF0C\u6211\u4EEC\u53EA\u6709`try-catch`\u5757\u3002\u867D\u7136\u4E0D\u662F\
  \u975E\u5E38\u7075\u6D3B\uFF0C\u4F46\u5B83\u5B8C\u6210\u4E86\u4EFB\u52A1\u3002 ES6\uFF08\
  2015\uFF09\u5F15\u5165\u4E86Promises\uFF0C\u5E76\u63D0\u4F9B\u4E86`.then()`\u548C\
  `.catch()`\uFF0C\u4F7F\u6211\u4EEC\u80FD\u591F\u66F4\u4F18\u96C5\u5730\u5904\u7406\
  \u5F02\u6B65\u9519\u8BEF\u3002"
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何处理：
这里是经典的`try-catch`块：

```javascript
try {
  // 可能会抛出错误的代码
  let result = potentiallyRiskyOperation();
  console.log('成功：', result);
} catch (error) {
  // 如果抛出错误应该怎么办
  console.error('糟糕：', error.message);
}
```

当没有错误发生时的示例输出：
```
成功：42
```

当发生错误时：
```
糟糕：出了些问题
```

对于涉及到promise的异步代码，请在`async`函数中使用`try-catch`：

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('数据获取成功：', data);
  } catch (error) {
    console.error('获取数据错误：', error.message);
  }
}

fetchData();
```

## 深入探讨
JavaScript中的错误处理已经发展。早在ES3时代（大约1999年），我们只有`try-catch`块。虽然不是非常灵活，但它完成了任务。

ES6（2015）引入了Promises，并提供了`.then()`和`.catch()`，使我们能够更优雅地处理异步错误。

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('数据获取成功：', data))
  .catch(error => console.error('获取数据错误：', error.message));
```

就实现细节而言，当抛出一个错误时，JavaScript引擎会创建一个带有诸如`message`和`stack`等有用属性的`Error`对象。您还可以通过扩展`Error`类来制作自定义错误类型——这对于更复杂的应用程序来说很方便。

其他选择？您可以忽略错误处理（坏主意），使用具有错误优先参数的回调（你好，Node.js风格），或者使用提供他们看法的库和框架来进行更高级的处理。

## 另见
有关错误处理的更多信息：

- MDN上的try-catch：[MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await：[MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- Promises指南：[MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- 创建和抛出自定义错误：[MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
