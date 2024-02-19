---
aliases:
- /zh/bash/working-with-toml/
date: 2024-01-26 04:19:02.812086-07:00
description: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u660E\
  \u4E86\u3001\u7B80\u7EA6\u7684\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u6570\u636E\
  \u5E8F\u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u56E0\u5176\u7B80\u5355\u6613\
  \u8BFB\u800C\u559C\u7231\u5B83\uFF1B\u5B83\u975E\u5E38\u9002\u5408\u914D\u7F6E\u6587\
  \u4EF6\uFF0C\u6709\u70B9\u50CFYAML\u7684\u611F\u89C9\uFF0C\u4F46\u5BF9\u4E8E\u4EBA\
  \u7C7B\u6765\u8BF4\u6BD4JSON\u66F4\u4E0D\u90A3\u4E48\u7E41\u7410\u3002"
lastmod: 2024-02-18 23:08:59.313157
model: gpt-4-0125-preview
summary: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u660E\u4E86\
  \u3001\u7B80\u7EA6\u7684\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u6570\u636E\u5E8F\
  \u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u56E0\u5176\u7B80\u5355\u6613\u8BFB\
  \u800C\u559C\u7231\u5B83\uFF1B\u5B83\u975E\u5E38\u9002\u5408\u914D\u7F6E\u6587\u4EF6\
  \uFF0C\u6709\u70B9\u50CFYAML\u7684\u611F\u89C9\uFF0C\u4F46\u5BF9\u4E8E\u4EBA\u7C7B\
  \u6765\u8BF4\u6BD4JSON\u66F4\u4E0D\u90A3\u4E48\u7E41\u7410\u3002"
title: "\u4F7F\u7528TOML"
---

{{< edit_this_page >}}

## 什么 & 为什么？
TOML，即Tom's Obvious, Minimal Language（汤姆明了、简约的语言），是一种数据序列化格式。程序员因其简单易读而喜爱它；它非常适合配置文件，有点像YAML的感觉，但对于人类来说比JSON更不那么繁琐。

## 如何操作：
首先，安装 `toml-cli` 以在Bash中使用TOML。对于即时读取或编辑TOML文件很方便。

```Bash
# 安装 toml-cli，我们处理TOML任务的小帮手
pip install toml-cli

# 假设你有一个TOML文件，'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# 读取一个值
toml get config.toml owner.name
# 输出: Tom

# 设置一个值
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# 专业提示：对于带点或奇怪字符的键，请使用引号！
```

## 深入了解
TOML在2013年左右诞生，源于对JSON对人类不友好的障碍的不满。GitHub的联合创始人Tom Preston-Werner想要某种非常易于阅读的东西。YAML和INI是替代品，但TOML像是两者的最佳结合。

突然间，你有了嵌套数据和数组，减少了YAML的脚枪和JSON的大括号。 TOML现在是Rust的Cargo配置中的首选，这表明了它在开发界的崛起。它由一个规范驱动，保持事物紧密且定义明确。你几乎可以在任何语言中获取解析器，这使得它被广泛采纳。

## 另请参阅
- 官方TOML GitHub仓库：https://github.com/toml-lang/toml
- PyPI上的 toml-cli：https://pypi.org/project/toml-cli/
- 数据序列化格式比较：https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
