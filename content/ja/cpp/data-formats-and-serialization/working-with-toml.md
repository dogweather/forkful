---
title:                "TOMLを扱う方法"
aliases:
- ja/cpp/working-with-toml.md
date:                  2024-01-26T04:19:59.012426-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となぜ？
TOML（Tom's Obvious, Minimal Language）は、その明確なセマンティクスによって読みやすいデータ直列化形式です。プログラマーは、人間の可読性と機械の解析可能性の間のバランスを取るために、設定ファイルにTOMLを使用します。

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
