---
date: 2024-01-26 04:19:59.012426-07:00
description: "TOML\uFF08Tom's Obvious, Minimal Language\uFF09\u306F\u3001\u305D\u306E\
  \u660E\u78BA\u306A\u30BB\u30DE\u30F3\u30C6\u30A3\u30AF\u30B9\u306B\u3088\u3063\u3066\
  \u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u5F62\u5F0F\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4EBA\u9593\u306E\u53EF\
  \u8AAD\u6027\u3068\u6A5F\u68B0\u306E\u89E3\u6790\u53EF\u80FD\u6027\u306E\u9593\u306E\
  \u30D0\u30E9\u30F3\u30B9\u3092\u53D6\u308B\u305F\u3081\u306B\u3001\u8A2D\u5B9A\u30D5\
  \u30A1\u30A4\u30EB\u306BTOML\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.585183-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF08Tom's Obvious, Minimal Language\uFF09\u306F\u3001\u305D\u306E\u660E\
  \u78BA\u306A\u30BB\u30DE\u30F3\u30C6\u30A3\u30AF\u30B9\u306B\u3088\u3063\u3066\u8AAD\
  \u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u5F62\u5F0F\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4EBA\u9593\u306E\u53EF\u8AAD\
  \u6027\u3068\u6A5F\u68B0\u306E\u89E3\u6790\u53EF\u80FD\u6027\u306E\u9593\u306E\u30D0\
  \u30E9\u30F3\u30B9\u3092\u53D6\u308B\u305F\u3081\u306B\u3001\u8A2D\u5B9A\u30D5\u30A1\
  \u30A4\u30EB\u306BTOML\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

## 使い方:
C++でTOMLを扱うには、`toml++`のようなライブラリが必要です。早速始めましょう：

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // ファイルからTOMLを解析
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // 値にアクセスする
    std::string title = config["title"].value_or("無題");
    std::cout << "タイトル: " << title << '\n';

    // TOMLを変更して保存
    config["title"] = "新しいタイトル";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

`config.toml`のサンプル：
```toml
title = "例"
```

サンプル出力：
```plaintext
タイトル: 例
```

## 詳細分析
TOMLはTom Preston-Wernerによって2013年にYAMLやJSONへの代替として作成されました。特に設定ファイル用として、シンプルかつ明示的な設計がされています。JSONと違って、TOMLはあいまいさがなく、文書の解析方法が決定論的である点に焦点を当てています。

TOMLの代替としては、許可されていることが多いものの時々予測可能性のコストがかかるYAMLや、構造が厳格であるがコメント不足と括弧の多用により設定において人間に優しくないJSONなどがあります。

実装において、`toml++`は最新のTOML仕様に準拠したC++17のヘッダーのみのライブラリです。DOMのようなインターフェースを提供してTOMLデータをナビゲートし、操作することを容易にし、プロジェクトに統合しやすくしています。ライブラリは、パース、検証、および出力生成を担当し、C++の型を使用してTOMLデータを取得および設定することを可能にします。

## 参照
- TOML GitHubリポジトリ: https://github.com/toml-lang/toml
- `toml++`, TOML用のC++ライブラリ: https://github.com/marzer/tomlplusplus
- 公式TOMLドキュメントと形式の詳細な説明: https://toml.io/ja/
