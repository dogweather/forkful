---
title:                "jsonの扱い方"
html_title:           "Fish Shell: jsonの扱い方"
simple_title:         "jsonの扱い方"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ

JSONはデータの交換に使用される一般的なフォーマットです。このフォーマットは非常に人間にとって読みやすく、機械可読性も高いため、プログラミングで使用する必要があります。

## 使い方

まず、Fish Shellをダウンロードしてインストールします。次に、JSONパッケージをインストールします。

```Fish Shell
brew install jq
```

次に、サンプルのJSONファイルを作成します。

```Fish Shell
echo '{"name": "John", "age": 30}' > sample.json
```

これで、`sample.json`ファイルが作成されました。次に、Fish Shellで以下のコマンドを実行してJSONファイルの内容を確認します。

```Fish Shell
jq . sample.json
```

これにより、JSONファイルの内容が表示されます。もし特定のキーの値だけを取得したい場合は、次のようにコマンドを変更します。

```Fish Shell
jq .name sample.json
```

これにより、`name`キーの値である`John`が表示されます。

## ディープダイブ

Fish Shellでは、より高度なJSONの処理が可能です。例えば、`jq`コマンドを使用してJSONファイル内の特定の値をフィルタリングすることができます。また、配列やオブジェクト内の値に対しても操作することができます。詳細な使い方やコマンドのオプションについては、[公式ドキュメント](https://stedolan.github.io/jq/)を参照してください。

## 参考リンク

- [Fish Shell](https://fishshell.com/)
- [JSON Format](https://www.json.org/json-en.html)
- [jq Official Documentation](https://stedolan.github.io/jq/)