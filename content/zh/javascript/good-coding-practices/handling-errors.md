---
date: 2024-01-26 00:54:42.799525-07:00
description: "\u9519\u8BEF\u5904\u7406\u662F\u6307\u5728\u4EE3\u7801\u6267\u884C\u4E0D\
  \u6B63\u5E38\u65F6\u60A8\u7BA1\u7406\u8FD9\u4E9B\u95EE\u9898\u7684\u65B9\u5F0F\u3002\
  \u5B83\u975E\u5E38\u5173\u952E\uFF0C\u56E0\u4E3A\u9519\u8BEF\u5904\u7406\u5E2E\u52A9\
  \u60A8\u7684\u7A0B\u5E8F\u4F18\u96C5\u5730\u5931\u8D25\u5E76\u6E05\u6670\u5730\u6307\
  \u5BFC\u7528\u6237\uFF0C\u800C\u4E0D\u662F\u7B80\u5355\u5730\u5D29\u6E83\u548C\u71C3\
  \u70E7\u3002"
lastmod: 2024-02-19 22:05:07.276248
model: gpt-4-1106-preview
summary: "\u9519\u8BEF\u5904\u7406\u662F\u6307\u5728\u4EE3\u7801\u6267\u884C\u4E0D\
  \u6B63\u5E38\u65F6\u60A8\u7BA1\u7406\u8FD9\u4E9B\u95EE\u9898\u7684\u65B9\u5F0F\u3002\
  \u5B83\u975E\u5E38\u5173\u952E\uFF0C\u56E0\u4E3A\u9519\u8BEF\u5904\u7406\u5E2E\u52A9\
  \u60A8\u7684\u7A0B\u5E8F\u4F18\u96C5\u5730\u5931\u8D25\u5E76\u6E05\u6670\u5730\u6307\
  \u5BFC\u7528\u6237\uFF0C\u800C\u4E0D\u662F\u7B80\u5355\u5730\u5D29\u6E83\u548C\u71C3\
  \u70E7\u3002"
title: "\u5904\u7406\u9519\u8BEF"
---

{{< edit_this_page >}}

## 何为“错误处理”以及为什么要进行错误处理？

错误处理是指在代码执行不正常时您管理这些问题的方式。它非常关键，因为错误处理帮助您的程序优雅地失败并清晰地指导用户，而不是简单地崩溃和燃烧。

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
