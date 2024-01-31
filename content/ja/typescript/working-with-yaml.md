---
title:                "YAMLを扱う"
date:                  2024-01-19
simple_title:         "YAMLを扱う"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
YAMLは人間にも機械にも読みやすいデータ表現フォーマットです。設定ファイルやデータ交換によく使われ、JSONよりも読みやすく、編集しやすいからです。

## How to: (使い方)
TypeScriptでYAMLを扱うには`js-yaml`ライブラリが便利です。

```typescript
// 必要なパッケージをインストール
// npm i js-yaml @types/js-yaml

import * as yaml from 'js-yaml';
import * as fs from 'fs';

// YAMLファイルの読み込みと解析
const doc = yaml.load(fs.readFileSync('config.yaml', 'utf8'));
console.log(doc);

// JavaScriptオブジェクトをYAMLファイルに変換して書き込み
const data = { title: 'YAML Example', count: 3 };
fs.writeFileSync('output.yaml', yaml.dump(data), 'utf8');
```

サンプル出力：
```yaml
title: YAML Example
count: 3
```

## Deep Dive (詳細解説)
YAMLは"YAML Ain't Markup Language"の略で、データが構造化された形式を持つことができます。JSONやXMLと違い、インデントを用いて階層構造を表現します。`js-yaml`ライブラリ以外にも`yaml`や`node-yaml`などの代替ライブラリが存在します。しかし、`js-yaml`はその安定性、速度、柔軟性から多くのプロジェクトで採用されています。

## See Also (参照)
- YAML公式サイト: [https://yaml.org](https://yaml.org)
- `js-yaml` GitHubレポジトリ: [https://github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
- TypeScript公式サイト: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)
