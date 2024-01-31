---
title:                "YAMLを扱う"
date:                  2024-01-19
simple_title:         "YAMLを扱う"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
YAMLは設定ファイルやデータ交換で人気。読み書き簡単だから。SwiftでYAMLを扱うことはアプリの設定や外部データの処理で役立つ。

## How to: (やり方：)
SwiftでYAMLをパースするには、`Yams`ライブラリが便利だ。`Yams`のインストールと簡単な使用例を示す。

```Swift
import Yams

let yamlString = """
name: Yukihiro Matsumoto
nickname: Matz
languages:
  - Ruby
  - Perl
  - C
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
    }
} catch {
    print("Error parsing YAML: \(error)")
}
```

出力：
```
["name": "Yukihiro Matsumoto", "nickname": "Matz", "languages": ["Ruby", "Perl", "C"]]
```

## Deep Dive (詳細解説)
YAMLは"YAML Ain't Markup Language"の略。データシリアライズフォーマットとして2001年に登場。JSONやXMLの代わりに使うことも可能。Swiftでは`Codable`プロトコルでYAMLサポートを追加可能。

## See Also (関連する情報)
- Yams GitHub: https://github.com/jpsim/Yams
- YAML公式サイト: https://yaml.org
- Swift公式サイト: https://swift.org

読むの忘れずに。もっと詳細知りたかったら、これらのリンクが役に立つはずだ。
