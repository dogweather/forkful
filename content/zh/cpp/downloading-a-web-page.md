---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么和为什么？

下载网页是获取网页源代码的过程，并以文件的形式存储在您的设备上。程序员这样做是因为他们可以离线检查源代码，分析其结构和内容，或将其用于数据挖掘和网络爬行等。

## 怎么样：

我们将使用C++中的库CPR(即Curl for People)来下载网页。这里有一个基本的代码例子：

```C++
#include <cpr/cpr.h>
int main(){
    cpr::Response r = cpr::Get(cpr::Url{"http://www.example.com"});
    std::ofstream fout("example.html");
    fout << r.text;
}
```

运行此代码后，您将在同一目录中找到一个名为"example.html"的文件，该文件包含从web页面"www.example.com"下载的源代码。

## 深度学习：

(1) 历史背景：在许多年前的早期互联网时代，Web浏览器的主要任务就是下载和显示网页。从那时起，我们已经看到了各种工具和库的发展，比如curl和wget，它们可以帮助编程语言（包括C++）下载网页。

(2) 选择替代方案：虽然本文提到的CPR库非常容易使用，但是还有其他一些库也可以达到同样的目的，比如Poco和Boost.Asio。

(3) 实现细节：当我们谈到下载网页时，实际上我们是在发送HTTP GET请求到服务器，然后接收返回的数据。这是web浏览器如何获取和显示网页的方式。

## 另请参阅：

1. CPR库的GitHub页面：https://github.com/whoshuu/cpr
2. 详细的HTTP GET和POST请求解释：https://www.w3schools.com/tags/ref_httpmethods.asp
3. C++中的其他库Poco：https://pocoproject.org/ 和 Boost.Asio：https://www.boost.org/doc/libs/1_63_0/doc/html/boost_asio.html
4. 用于同样目的的其他编程语言如Python中用于下载网页的库有urllib和requests等。