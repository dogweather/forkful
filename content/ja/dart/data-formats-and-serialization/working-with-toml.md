---
title:                "TOMLを使った作業"
date:                  2024-03-08T21:57:29.975667-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

TOML、または Tom's Obvious, Minimal Language（トムの明白で最小の言語）は、明確なセマンティクスのために読みやすい設定ファイルフォーマットです。プログラマーは、パースが直接的で、混乱やエラーが最小限に抑えられるため、ソフトウェアアプリケーションを設定するために使用します。

## 方法：

DartにはTOMLの組み込みサポートは含まれていませんが、`toml`のようなサードパーティのパッケージを使用してTOMLファイルを扱うことができます。まず、`pubspec.yaml`に`toml`を追加します：

```yaml
dependencies:
  toml: ^0.10.0
```

### TOMLの読み取り

TOMLファイルを読み取るために、簡単な設定ファイル`config.toml`を持っていると仮定しましょう：

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

DartでこのTOMLファイルを解析するには次のようにします：

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // 'database'セクションを出力
}
```

これは以下を出力します：

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### TOMLの作成

TOMLコンテンツを作成するには、`toml`パッケージによって提供される`TomlBuilder`を使用します：

```dart
import 'package:toml/toml.dart';

void main() {
  final builder = TomlBuilder();

  builder.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = builder.build().toString();
  print(tomlString);
}
```

これにより、`config.toml`ファイルと非常に似たTOMLコンテンツの文字列表現が生成され、印刷されます：

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

これらの例は、Dartアプリケーションで設定データを扱う際に、TOMLファイルから読み取り、書き込む方法を示しています。
