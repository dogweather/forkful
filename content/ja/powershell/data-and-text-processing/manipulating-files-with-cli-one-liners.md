---
date: 2024-01-27 16:21:06.029921-07:00
description: "PowerShell\u3067CLI\u30EF\u30F3\u30E9\u30A4\u30CA\u30FC\u3092\u4F7F\u3063\
  \u3066\u30D5\u30A1\u30A4\u30EB\u3092\u64CD\u4F5C\u3059\u308B\u3068\u3044\u3046\u306E\
  \u306F\u3001\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u304B\u3089\u76F4\u63A5\u3001\
  \u8FC5\u901F\u306B\u30D5\u30A1\u30A4\u30EB\u306E\u30C7\u30FC\u30BF\u3092\u5909\u66F4\
  \u3001\u79FB\u52D5\u3001\u307E\u305F\u306F\u53D6\u5F97\u3059\u308B\u3053\u3068\u306B\
  \u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u52B9\
  \u7387\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002GUI\u3092\
  \u64CD\u4F5C\u3059\u308B\u3088\u308A\u3082\u901F\u304F\u3001\u5358\u7D14\u306A\u30BF\
  \u30B9\u30AF\u306E\u305F\u3081\u306B\u9577\u3044\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\
  \u66F8\u304F\u3088\u308A\u3082\u901F\u3044\u306E\u3067\u3059\u3002"
lastmod: 2024-02-19 22:05:01.552980
model: gpt-4-0125-preview
summary: "PowerShell\u3067CLI\u30EF\u30F3\u30E9\u30A4\u30CA\u30FC\u3092\u4F7F\u3063\
  \u3066\u30D5\u30A1\u30A4\u30EB\u3092\u64CD\u4F5C\u3059\u308B\u3068\u3044\u3046\u306E\
  \u306F\u3001\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u304B\u3089\u76F4\u63A5\u3001\
  \u8FC5\u901F\u306B\u30D5\u30A1\u30A4\u30EB\u306E\u30C7\u30FC\u30BF\u3092\u5909\u66F4\
  \u3001\u79FB\u52D5\u3001\u307E\u305F\u306F\u53D6\u5F97\u3059\u308B\u3053\u3068\u306B\
  \u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u52B9\
  \u7387\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002GUI\u3092\
  \u64CD\u4F5C\u3059\u308B\u3088\u308A\u3082\u901F\u304F\u3001\u5358\u7D14\u306A\u30BF\
  \u30B9\u30AF\u306E\u305F\u3081\u306B\u9577\u3044\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\
  \u66F8\u304F\u3088\u308A\u3082\u901F\u3044\u306E\u3067\u3059\u3002"
title: "CLI\u30EF\u30F3\u30E9\u30A4\u30CA\u30FC\u3067\u306E\u30D5\u30A1\u30A4\u30EB\
  \u64CD\u4F5C"
---

{{< edit_this_page >}}

## 何となぜ？

PowerShellでCLIワンライナーを使ってファイルを操作するというのは、コマンドラインから直接、迅速にファイルのデータを変更、移動、または取得することについてです。プログラマーは効率のためにこれを行います。GUIを操作するよりも速く、単純なタスクのために長いスクリプトを書くよりも速いのです。

## 方法：

### ファイルの読み取り
ファイルの内容をすばやく表示するには、`Get-Content` コマンドを使用します：
```PowerShell
Get-Content .\example.txt
```

### ファイルへの書き込み
ファイルに新しいものを書き込むには、`Set-Content` を使用できます：
```PowerShell
Set-Content -Path .\example.txt -Value "Hello, PowerShell!"
```

### ファイルへの追記
ファイルの内容を消去することなく末尾にデータを追加するには、`Add-Content` を使用します：
```PowerShell
Add-Content -Path .\example.txt -Value "Adding this line."
```

### ファイルのコピー
ファイルをコピーするのは、`Copy-Item` を使って簡単です：
```PowerShell
Copy-Item -Path .\example.txt -Destination .\copy_of_example.txt
```

### ファイルの削除
ファイルを削除するには、単に `Remove-Item` を使用します：
```PowerShell
Remove-Item -Path .\unwanted_file.txt
```

### ファイル内の検索
ファイル内のテキストを検索するには `Select-String` を使用します：
```PowerShell
Select-String -Path .\*.txt -Pattern "PowerShell"
```

### コマンドの組み合わせ
PowerShellはコマンドをパイプで連鎖させる能力で本当に際立ちます。新しいディレクトリにファイルを見つけてコピーする方法は以下のとおりです：
```PowerShell
Get-ChildItem -Path .\*.log | Copy-Item -Destination C:\Logs
```

## 深堀り

歴史的に見ると、PowerShellはWindowsの伝統的なコマンドプロンプトへのより強力な代替として導入されました。これは、システムの内部およびデータストアへの前例のないアクセスを提供します。PowerShellはコマンドラインの速度とスクリプトの柔軟性を組み合わせ、Windowsベースのシステム管理者や開発者にとって非常に貴重なツールとなっています。

PowerShellの代替手段には、Unixベースのツールである`sed`、`awk`、`grep`、LinuxやMacOSユーザー向けの`bash`スクリプティングなどがあります。これらのツールは非常に強力であり、それぞれの長所がありますが、PowerShellはWindows環境との深い統合を提供します。

PowerShellの注目すべき側面はそのオブジェクト指向性です。多くのスクリプティング言語はすべてを文字列やバイトのストリームとして扱いますが、PowerShellは.NETオブジェクトで直接作業します。これは、ファイルを操作するときに、多くのプロパティとメソッドを提供する豊かなオブジェクトを使って作業していることを意味し、複雑なタスクをより管理しやすくしています。

特にLinuxやMacOSユーザーにとって、PowerShellの欠点の一つは、bashスクリプティングやUnixコマンドラインツールの使用に比べて、その冗長性が認識されることです。さらに、PowerShellのWindowsとの深い統合は、時にはクロスプラットフォームスクリプトを少し挑戦的にすることがありますが、PowerShell Coreの努力はその隔たりを効果的に橋渡しすることを目指しています。

その弱点にかかわらず、PowerShellの強みは強力なワンライナー機能、統合スクリプティング環境、そしてWindowsエコシステムへの包括的なアクセスを提供することにあり、コマンドラインから直接ファイルを操作してそれ以上のことをしたい人にとって欠かせないツールとなっています。
