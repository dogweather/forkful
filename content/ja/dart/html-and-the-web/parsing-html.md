---
title:                "HTMLの構文解析"
date:                  2024-03-08T21:55:20.699709-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
プログラミングにおけるHTMLの解析は、HTMLドキュメントからデータを抽出することを意味します。プログラマーは、公式APIが利用できない場合でも、ウェブコンテンツとやり取りをしたり、情報抽出、テスト、あるいは自動化の目的でウェブコンテンツをスクレイピングするためにこれを行います。

## 方法：
Dartでは、そのコアライブラリにHTML解析をサポートする機能を組み込んでいません。しかし、`html`のようなサードパーティパッケージを使用してHTMLドキュメントを解析し、操作することができます。

まず、`html`パッケージを`pubspec.yaml`ファイルに追加します：

```yaml
dependencies:
  html: ^0.15.0
```

次に、パッケージをDartファイルにインポートします：

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

以下は、HTMLを含む文字列を解析し、データを抽出する基本的な例です：

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hello, Dart!</h1>
      <p>これはサンプルHTMLの段落です</p>
    </body>
  </html>
  """;

  // HTML文字列を解析
  Document document = parse(htmlDocument);

  // データを抽出
  String title = document.querySelector('h1')?.text ?? "タイトルが見つかりません";
  String paragraph = document.querySelector('p')?.text ?? "段落が見つかりません";

  print('タイトル: $title');
  print('段落: $paragraph');
}
```

出力：

```
タイトル: Hello, Dart!
段落: これはサンプルHTMLの段落です
```

実際のウェブページとやり取りする場合は、HTTPリクエスト（`http`パッケージを使用してウェブコンテンツをフェッチすること）と`html`解析を組み合わせることがあります。ここに簡単な例を示します：

まず、`html`とともに`http`パッケージを追加します：

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

次に、ウェブからHTMLページをフェッチして解析します：

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // ウェブページをフェッチ
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // ページに興味のある<h1>タグがあると仮定
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('ヘッドライン: $headlines');
  } else {
    print('リクエストはステータスで失敗しました: ${response.statusCode}。');
  }
}
```

注意：上で示されたウェブスクレイピング技術は、ウェブサイトの利用規約に従って責任を持って使用すべきです。
