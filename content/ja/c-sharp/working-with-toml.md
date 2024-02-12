---
title:                "TOMLを扱う方法"
aliases:
- ja/c-sharp/working-with-toml.md
date:                  2024-01-26T04:20:32.055725-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となぜ？
TOMLはTom's Obvious, Minimal Languageの略で、その明快なセマンティクスのおかげで読みやすい設定ファイル形式です。プログラマーは設定ファイルに使用し、システム間のデータ交換を簡素化し、人間の可読性とマシンの解析可能性の間のバランスを取るために利用しています。

## 方法：
まず、`Tomlyn`のようなTOMLパーサーをインストールします。パッケージマネージャーを使います：

```csharp
dotnet add package Tomlyn
```

次に、TOMLファイルを解析します：

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"所有者: {tomlTable["owner"]["name"]}");
// 出力：
// 所有者: Tom Preston-Werner
```

今度は、TOMLを作成し、書き込みます：

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOMLをconfig.tomlに書き込みました");
// 出力：
// TOMLをconfig.tomlに書き込みました
```

## 深掘り：
TOMLは2013年頃に、GitHubの共同設立者であるTom Preston-Wernerによって、YAMLやJSONのような既存の形式の設定における限界に対する反応として作られました。設定用に特化しており、直截明快で曖昧さのないことを重視しています。

代替設定形式にはYAML、JSON、XMLがあります。しかし、特に手作業での編集が一般的な設定ファイルにおいて、TOMLはより人に優しい形式で際立っています。JSONは普及していますが、複雑な設定では読みにくく、XMLは冗長です。YAMLは可読性においては似ていますが、空白の使用が多くなると複雑になり、特定の内容ではセキュリティリスクがあります。

実装面では、TOMLはハッシュテーブルへのクリーンなマッピングに焦点を当てており、データの抽出が予測可能です。バージョン1.0.0がリリースされると、TOMLはその仕様を固め、安定性とツールのサポートを改善しました。

## 参照：
- 公式のTOML GitHubリポジトリ＆スペック：[github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- .NETライブラリのTomlyn：[github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
