---
date: 2024-01-26 04:19:02.812086-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u9996\u5148\uFF0C\u5B89\u88C5 `toml-cli`\
  \ \u4EE5\u5728Bash\u4E2D\u4F7F\u7528TOML\u3002\u5BF9\u4E8E\u5373\u65F6\u8BFB\u53D6\
  \u6216\u7F16\u8F91TOML\u6587\u4EF6\u5F88\u65B9\u4FBF\u3002"
lastmod: '2024-04-05T22:38:47.144437-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u9996\u5148\uFF0C\u5B89\u88C5 `toml-cli`\
  \ \u4EE5\u5728Bash\u4E2D\u4F7F\u7528TOML\u3002\u5BF9\u4E8E\u5373\u65F6\u8BFB\u53D6\
  \u6216\u7F16\u8F91TOML\u6587\u4EF6\u5F88\u65B9\u4FBF\u3002"
title: "\u4F7F\u7528TOML"
weight: 39
---

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
