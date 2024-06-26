---
date: 2024-01-27 16:20:59.214435-07:00
description: "\u65B9\u6CD5\uFF1A \u30E6\u30FC\u30B6\u30FC\u30D5\u30EC\u30F3\u30C9\u30EA\
  \u30FC\u306A\u6A5F\u80FD\u3068\u5F37\u529B\u306A\u30B9\u30AF\u30EA\u30D7\u30C6\u30A3\
  \u30F3\u30B0\u6A5F\u80FD\u3067\u77E5\u3089\u308C\u308B Fish Shell \u306F\u3001\u305D\
  \u306E\u5834\u3067\u30D5\u30A1\u30A4\u30EB\u3092\u7DE8\u96C6\u3059\u308B\u3044\u304F\
  \u3064\u304B\u306E\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\
  \u305F\u3060\u3057\u3001\u4ED6\u306E\u30B7\u30A7\u30EB\u3068\u306F\u7570\u306A\u308A\
  \u3001Fish \u306B\u306F\u305D\u306E\u5834\u3067\u306E\u7DE8\u96C6\u306E\u305F\u3081\
  \u306E\u7D44\u307F\u8FBC\u307F\u30E1\u30AB\u30CB\u30BA\u30E0\u306F\u3042\u308A\u307E\
  \u305B\u3093\uFF08\u4F8B\u3048\u3070 Bash \u306E `sed -i`\u2026"
lastmod: '2024-04-05T22:38:42.217630-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u30E6\u30FC\u30B6\u30FC\u30D5\u30EC\u30F3\u30C9\u30EA\
  \u30FC\u306A\u6A5F\u80FD\u3068\u5F37\u529B\u306A\u30B9\u30AF\u30EA\u30D7\u30C6\u30A3\
  \u30F3\u30B0\u6A5F\u80FD\u3067\u77E5\u3089\u308C\u308B Fish Shell \u306F\u3001\u305D\
  \u306E\u5834\u3067\u30D5\u30A1\u30A4\u30EB\u3092\u7DE8\u96C6\u3059\u308B\u3044\u304F\
  \u3064\u304B\u306E\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\
  \u305F\u3060\u3057\u3001\u4ED6\u306E\u30B7\u30A7\u30EB\u3068\u306F\u7570\u306A\u308A\
  \u3001Fish \u306B\u306F\u305D\u306E\u5834\u3067\u306E\u7DE8\u96C6\u306E\u305F\u3081\
  \u306E\u7D44\u307F\u8FBC\u307F\u30E1\u30AB\u30CB\u30BA\u30E0\u306F\u3042\u308A\u307E\
  \u305B\u3093\uFF08\u4F8B\u3048\u3070 Bash \u306E `sed -i` \uFF09\u3002\u3057\u304B\
  \u3057\u5FC3\u914D\u7121\u7528\u3001\u5C11\u3057\u306E\u5275\u9020\u6027\u3068 `sed`\
  \ \u3084 `awk` \u306A\u3069\u306E\u5916\u90E8\u30C4\u30FC\u30EB\u306E\u52A9\u3051\
  \u3092\u501F\u308A\u3066\u3001\u3053\u308C\u3092\u5B9F\u73FE\u3059\u308B\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002"
title: "CLI\u30EF\u30F3\u30E9\u30A4\u30CA\u30FC\u3067\u306E\u30D5\u30A1\u30A4\u30EB\
  \u306E\u30A4\u30F3\u30D7\u30EC\u30FC\u30B9\u7DE8\u96C6"
weight: 32
---

## 方法：
ユーザーフレンドリーな機能と強力なスクリプティング機能で知られる Fish Shell は、その場でファイルを編集するいくつかの方法を提供しています。ただし、他のシェルとは異なり、Fish にはその場での編集のための組み込みメカニズムはありません（例えば Bash の `sed -i` ）。しかし心配無用、少しの創造性と `sed` や `awk` などの外部ツールの助けを借りて、これを実現することができます。

### `sed` を使用した単純な置換
`file.txt` の "hello" を全て "world" に置換するには、以下を使用します：
```Fish Shell
sed -i '' 's/hello/world/g' file.txt
```

### 複数の `sed` コマンドを適用する
複数の置換を行う必要がある場合、それらをこのように連鎖させることができます：
```Fish Shell
sed -i '' -e 's/fish/bass/g' -e 's/rainbow/trout/g' file.txt
```

### より複雑な操作に `awk` を使用する
`sed` では複雑すぎる操作には、`awk` がおすすめのツールかもしれません。ここでは、各行の数字を倍にする方法を示します：
```Fish Shell
awk '{print $1 * 2}' file.txt > temp && mv temp file.txt
```

### エラーハンドリングについての注意
これらのツールを Fish から使用する際には、エラーのキャプチャとそのメッセージを理解することが重要です。Fish の堅牢なエラーハンドリングを使用して、スクリプトをより信頼性の高いものにしてください。

## 深堀り
歴史的に、その場でのファイル編集は Unix および Linux プログラミングの基本であり、手動でファイルを開くことなく迅速に編集を行う効率的な方法を提供しています。`sed` や `awk` などのツールは Unix の初期から存在し、テキスト処理タスクに欠かせないユーティリティとなっています。

Fish Shell は、より現代的で使用性とスクリプティングの改善を誇っていますが、対話性とユーザーフレンドリーに焦点を当てた設計思想のために、組み込みのその場での編集機能を欠いています。Fish にその場での編集コマンドがネイティブに存在しないことは、Unix系エコシステム内の外部ツールの重要性を強調しています。

Fish でその場での編集の代替手段には、一時ファイルを使用するか、Perl や Python のワンライナーを活用する方法があり、これらは複雑なタスクに対してより多くの柔軟性や可読性を提供することができます。

たとえば、Perl を使用する場合：
```Fish Shell
perl -pi -e 's/find/replace/g' file.txt
```
または Python を使用する場合：
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('pattern', 'replacement', line)) for line in sys.stdin]" < file.txt > temp && mv temp file.txt
```

実装において、その場での編集を行う際、これらのツールは典型的には一時ファイルを作成し、変更されたバージョンで元のファイルを置き換える前にそこに変更を書き込みます。このアプローチは、操作中にエラーが発生した場合、ファイル編集プロセスがデータを損なうことや失うことがないように保証します。

これらのツールと方法を理解することにより、Fish Shell プログラマーは効果的にその場での編集をスクリプトに組み込むことができ、Fish のユーザーフレンドリーな機能と伝統的な Unix テキスト処理ユーティリティの生の力の間のギャップを埋めることができます。
