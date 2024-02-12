---
title:                "创建临时文件"
aliases: - /zh/google-apps-script/creating-a-temporary-file.md
date:                  2024-02-01T21:51:58.933852-07:00
model:                 gpt-4-0125-preview
simple_title:         "创建临时文件"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/creating-a-temporary-file.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Google Apps 脚本中创建临时文件涉及生成一个短期使用的文件，通常用于中间数据处理、调试或缓存目的。程序员这样做是为了临时管理数据，而不会使永久存储空间杂乱无章，或者在当前过程的范围之外，数据的永久性是不必要的。

## 如何操作：

在 Google Apps 脚本中，可以使用 DriveApp 服务来创建临时文件，该服务提供了一种简便的方法来创建、读取和删除 Google Drive 中的文件。以下是创建临时文本文件，向其写入一些数据，然后在使用后删除它的方法：

```javascript
function createTemporaryFile() {
  // 创建一个名为 "tempFile.txt" 的临时文件
  var tempFile = DriveApp.createFile('tempFile.txt', 'Temporary content', MimeType.PLAIN_TEXT);
  
  // 记录文件 URL 以便访问或调试
  Logger.log('创建临时文件：' + tempFile.getUrl());
  
  // 示例操作：读取文件内容
  var content = tempFile.getBlob().getDataAsString();
  Logger.log('tempFile 的内容：' + content);
  
  // 假设操作完成并且不再需要该文件
  // 删除临时文件
  tempFile.setTrashed(true);
  
  // 确认删除
  Logger.log('临时文件已删除');
}
```

运行此脚本将输出：

```
创建临时文件：[创建的临时文件的 URL]
tempFile 的内容：临时内容
临时文件已删除
```

这个示例脚本展示了创建临时文件、执行读取其内容的操作，最后，删除文件以进行清理的过程。

## 深入了解

在软件开发中创建临时文件的概念与文件管理本身的概念一样古老。在传统的文件系统中，临时文件通常在指定的临时目录中创建，并且对于各种中间过程至关重要，如对大型数据集进行排序、持有 web 应用程序的会话数据，或在文件转换过程中存储数据块。

在 Google Apps 脚本中，创建临时文件的过程利用了 Google Drive 的基础设施，它提供了云基础的文件管理与传统编程概念的有趣融合。然而，这种在 Google Drive 中创建临时文件的方法并非没有限制和成本，考虑到 Google Drive 强加的配额限制。此外，与本地文件系统相比，通过网络访问 Google Drive 的延迟可能是高性能应用程序的一个关键因素。

作为替代方案，开发人员可能会考虑在需要在计算期间临时存储小型数据集的情况下使用 Google Sheets，或者对于需要高性能读/写操作和更大存储容量的应用程序，考虑使用 Google Cloud Storage。每种解决方案都提供了关于延迟、存储限制和从 Google Apps 脚本使用的便利性不同的权衡。最终的选择取决于应用程序的具体要求以及它所在的现有基础设施。
