---
title:                "C: 从网页下载"
simple_title:         "从网页下载"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么

当我们在浏览器上浏览网页时，我们可能会想要保存下来以便稍后再查看。通过学习如何编写代码来下载网页，我们可以轻松地保存网页，并在没有互联网连接的情况下查看它们。

## 如何

首先，我们需要引入 stdio.h 和 stdlib.h 头文件，以便在程序中使用标准输入输出函数和动态内存分配函数。然后，我们需要定义一个函数来下载网页，如下所示：

```C
void download_page(char* url) {
	// 在此处编写代码来下载网页
}
```

接着，我们将在主函数中调用这个函数，并传入我们想要下载的网页的网址，如下所示：

```C
int main() {
	char* url = "https://www.example.com"; // 替换为你想要下载的网页的网址
	download_page(url); // 调用下载函数
}
```

现在，我们可以在下载函数中编写代码来实现网页下载的功能。一个简单的方法是使用标准C库中的 `system()` 函数来调用系统的 `wget` 命令来下载网页。代码如下所示：

```C
void download_page(char* url) {
	char command[50]; // 存储wget命令的数组
	sprintf(command, "wget %s", url); // 将网址插入wget命令中
	system(command); // 执行wget命令
}
```

当我们运行程序时，它将调用系统的 `wget` 命令来下载我们指定的网页。下载完成后，我们可以在程序所在的目录中找到名为 `index.html` 的文件，其中包含我们下载的网页的源代码和内容。

## 深入探讨

除了使用 `system()` 函数调用系统的 `wget` 命令，我们还可以使用标准C库中的 `fopen()` 和 `fputs()` 函数来直接从网址中读取并保存网页的内容。代码如下所示：

```C
void download_page(char* url) {

	// 使用文件指针来打开一个新文件，名为 "index.html"
	FILE* fp = fopen("index.html", "w");

	// 使用标准C库中的"文件指针"来读取并保存网页内容
	// 请注意，这将覆盖我们之前用系统的wget命令下载的同名文件
	FILE* web = fopen(url, "r");
	char c;
	while ((c = fgetc(web)) != EOF) { // 从网页中逐个读取字符
		fputc(c, fp); // 将每个字符写入名为 "index.html" 的文件中
	}
	fclose(web); // 关闭文件指针
	fclose(fp); // 关闭文件指针
}
```

使用这种方法，我们可以更精确地控制下载的网页内容，并在程序中进一步处理它们，例如提取特定信息或进行数据分析。

## 另请参阅

- [C语言教程](https://www.runoob.com/cprogramming/c-tutorial.html)
- [C标准库教程](https://www.runoob.com/cprogramming/c-standard-library.html)
- [用C语言做爬虫](https://zhuanlan.zhihu.com/p/382620065)