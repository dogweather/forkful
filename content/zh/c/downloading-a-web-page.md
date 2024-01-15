---
title:                "下载网页"
html_title:           "C: 下载网页"
simple_title:         "下载网页"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 为什么要下载网页？

下载网页是获取信息的一种常见方式。通过下载网页，用户可以获取网页上的文本、图像和其他媒体，从而满足他们的需求。

## 如何进行网页下载

网页下载可以通过使用C语言中的标准库中提供的功能来实现。首先，我们需要导入`<stdio.h>`和`<curl/curl.h>`头文件。然后，我们可以使用`CURL`结构体来创建一个可以执行下载任务的句柄。接下来，我们可以通过设置相应的选项，如`CURLOPT_URL`来指定要下载的网页地址。最后，使用`curl_easy_perform()`方法来执行下载任务，并将下载的内容保存到本地文件中。以下是一个简单的网页下载示例：

```
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  FILE *fp;
  CURLcode res;
  char *url = "https://example.com"; // 要下载的网页地址
  char outfilename[FILENAME_MAX] = "output.html"; // 保存下载内容的文件名

  curl = curl_easy_init(); // 初始化curl句柄
  if (curl)
  {
    fp = fopen(outfilename, "wb"); // 以二进制写入模式打开文件
    if (fp == NULL)
      return 0; // 打开文件失败，结束程序

    // 设置要下载的网页地址
    curl_easy_setopt(curl, CURLOPT_URL, url);

    // 将下载的内容保存到指定的文件中
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);

    // 执行下载任务
    res = curl_easy_perform(curl);

    // 检查下载结果
    if (res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed:%s\n", curl_easy_strerror(res));

    // 关闭文件
    fclose(fp);

    // 回收curl句柄
    curl_easy_cleanup(curl);
  }

  return 0;
}
```

运行上述代码后，会将指定网页的内容下载并保存为`output.html`文件，供用户随后查看和使用。

## 深入了解网页下载

使用C语言进行网页下载需要了解一些更深层次的知识。比如，我们可以通过设置不同的选项，来实现不同的功能，如设置`CURLOPT_USERAGENT`来伪装浏览器，让服务器认为我们是使用浏览器进行访问，而不是使用程序下载内容。此外，我们还可以使用`CURLcode`类型来处理下载过程中可能出现的错误，以保证程序的稳定性。

# 参考链接

- [CURL官方文档](https://curl.se/libcurl/c)
- [C语言标准库](https://www.cplusplus.com/reference/cstdlib/)
- [C语言教程](https://www.runoob.com/cprogramming/c-tutorial.html)

# 查看也可以

- [C语言中文网](https://c.biancheng.net/)
- [CURL库使用教程](https://www.jianshu.com/p/c7c95cb57be7)
- [CURL官方示例代码](https://curl.se/libcurl/c/example.html)