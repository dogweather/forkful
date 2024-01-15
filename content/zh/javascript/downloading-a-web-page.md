---
title:                "下载网页"
html_title:           "Javascript: 下载网页"
simple_title:         "下载网页"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么
为什么有人会想要下载一个网页？通常是因为他们想要保存网页的内容，以备以后离线使用或分享给其他人。

## 如何操作
网页下载可以通过Javascript中的内置函数`fetch()`来完成。该函数可以向指定的URL发出请求，并将响应内容以Promise的形式返回。下面是一个简单的示例：

```Javascript
fetch("https://example.com").then(response => {
    return response.text();
}).then(data => {
    console.log(data); // 输出网页的文本内容
});
```

如果想要下载网页的图片、音频或视频等媒体文件，可以使用`URL.createObjectURL()`方法来创建一个可下载的URL。示例如下：

```Javascript
fetch("https://example.com/image.jpg").then(response => {
    return response.blob();
}).then(data => {
    const downloadUrl = URL.createObjectURL(data); // 创建可下载的URL
    // 在页面中插入下载链接
    document.getElementById("download-link").setAttribute("href", downloadUrl);
});
```

## 深入细节
网页下载通常会涉及到跨域问题。如果要下载的网页与当前页面不在同一域名下，浏览器会拒绝跨域请求，因为这可能导致安全隐患。为了解决这个问题，可以在请求头中添加`Access-Control-Allow-Origin`字段来允许跨域请求。另外，使用`credentials: "include"`可以在请求中携带认证信息，例如cookie。

```Javascript
fetch("https://example.com/image.jpg", {
    headers: {"Access-Control-Allow-Origin": "*"}, // 允许跨域请求
    credentials: "include" // 携带认证信息
}).then(response => {
    return response.blob();
}).then(data => {
    const downloadUrl = URL.createObjectURL(data);
    document.getElementById("download-link").setAttribute("href", downloadUrl);
});
```

## 参考链接
- [MDN文档：`fetch()`函数](https://developer.mozilla.org/zh-CN/docs/Web/API/Fetch_API/Using_Fetch)
- [MDN文档：跨域资源共享（CORS）](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/CORS)
- [MDN文档：`URL.createObjectURL()`方法](https://developer.mozilla.org/zh-CN/docs/Web/API/URL/createObjectURL)