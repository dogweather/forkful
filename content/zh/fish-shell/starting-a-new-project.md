---
title:    "Fish Shell: 开始一个新项目"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

### 为什么要在Mandarin中阅读Fish Shell编程博客文章？
Fish Shell是一种强大的命令行环境，它具有简洁的语法和丰富的功能，让编程变得更加高效。阅读Mandarin的Fish Shell博客文章，可以帮助您更快地掌握这项技能，并在您的项目中更有效地使用它。

### 如何使用Fish Shell启动新项目
```Fish Shell
set -Ux PROJECT_NAME "My New Project"
mkdir $PROJECT_NAME
cd $PROJECT_NAME
touch main.py
echo "print('Hello World!')" >> main.py
python main.py
```
在上面的代码块中，我们使用set命令为项目设置一个变量，然后使用mkdir命令在当前路径下创建一个文件夹。接着，我们使用cd命令进入到该文件夹，并使用touch命令创建一个名为main.py的文件。最后，我们使用echo命令将一行代码写入main.py，并使用python命令运行该文件。这个简单的例子展示了如何使用Fish Shell创建并运行一个新的项目。

### 深入探讨启动新项目
在开始新项目之前，建议您使用/config命令来配置Fish Shell的设置，以符合您个人的偏好。您还可以使用help命令来了解更多可用的Fish Shell命令和功能。另外，您可以使用set命令来设置环境变量，以便在整个Fish Shell会话中使用。

### 参考链接
- Fish Shell官方网站：https://fishshell.com
- Fish Shell GitHub仓库：https://github.com/fish-shell/fish-shell
- Fish Shell配置指南：https://fishshell.com/docs/current/cmds/config.html
- Fish Shell帮助文档：https://fishshell.com/docs/current/cmds/help.html