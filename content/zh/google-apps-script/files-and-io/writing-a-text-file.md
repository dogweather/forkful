---
title:                "编写文本文件"
aliases: - /zh/google-apps-script/writing-a-text-file.md
date:                  2024-02-01T22:08:03.301808-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写文本文件"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/writing-a-text-file.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

在 Google Apps 脚本中写入文本文件允许开发者持久地储存数据，使其可供将来使用或分析。这种操作是记录、保存配置或以简单、易读格式导出信息的常见做法。

## 如何操作：

通过 Google DriveApp 服务，在 Google Apps 脚本中创建和写入文本文件可以实现。下面是一份附带代码示例的逐步指南，帮助你开始：

**步骤 1: 创建一个新的文本文件**

```javascript
// 在 Google Drive 的根目录中创建一个新的文本文件
var file = DriveApp.createFile('Example.txt', 'Hello, world!');
```

这段代码片段创建一个名为 "Example.txt"，内容为 "Hello, world!" 的文本文件。

**步骤 2: 打开并写入一个现有的文本文件**

如果你需要打开一个现有文件并写入内容，你可以使用 `getFileById(id)` 方法来检索文件，然后操作其内容。

```javascript
// 通过其 ID 获取一个文件并追加新内容
var fileId = 'YOUR_FILE_ID_HERE'; // 用你的实际文件 ID 替换 YOUR_FILE_ID_HERE
var file = DriveApp.getFileById(fileId);
file.setContent(file.getBlob().getDataAsString() + '\nNew content added.');
```

这段代码使用唯一 ID 检索一个现有文件，然后在先前内容之后追加 "New content added."。

**示例输出**

运行上述代码片段不会显示明确的输出，但如果你导航到存储文件的 Google Drive，你将看到第一个代码片段创建的 "Example.txt" 文件。对于第二个片段，如果你通过 ID 打开指定的文件，你应该会看到原始内容后面跟着新行 "New content added."。

## 深入探讨

在 Google Apps 脚本中写入文本文件利用了 DriveApp 服务，本质上利用了 Google Drive 的文件存储和管理能力。这种方法可以追溯到 Google Apps 脚本的诞生，它被设计为轻松自动化 Google 生产力工具套件（包括 Drive）的任务。

尽管通过 Google Apps 脚本直接操作文件直接并与 Google Workspace 紧密集成，但来自其他背景的开发者（例如，Python、Node.js）可能会发现这与在本地文件系统或其他云存储服务（如 AWS S3）上工作有所不同。这些平台通常提供一套更复杂的文件操作能力，但需要额外设置认证和权限。

对于需要超出简单文本文件（如二进制数据处理或广泛的文件系统操作）的更高级文件管理或处理能力的场景，开发者可能会考虑使用 Google 云平台服务（例如，Cloud Storage）与 Google Apps 脚本结合使用。尽管这些替代方法更强大，但也引入了更陡峭的学习曲线和潜在更高的成本，具体取决于项目的范围。

总而言之，虽然 Google Apps 脚本提供了一种易于访问且高效的方式来管理 Google Drive 内的文件，包括写入文本文件，但重要的是要了解其局限性，并在需要满足更复杂的要求时探索其他 Google 技术。
