---
title:                "連想配列の使用"
date:                  2024-01-30T19:12:45.608070-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"

category:             "PowerShell"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

PowerShellにおける連想配列（またはハッシュテーブル、辞書とも呼ばれる）を使うと、データをキーと値のペアで格納でき、データの取得を直接的で効率的に行えます。プログラマーはこれを使用して、キーによって簡単にアクセスできる方法で関連データを一緒に格納します。

## 使い方：

PowerShellで連想配列を作成して使用するのは非常に簡単です。こちらがその魔法の方法です：

**連想配列の作成：**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "エンジニア"
```

このコードスニペットは、3つのキーと値のペアを持つ連想配列を作成します。

**値にアクセスする：**

値を取得するには、そのキーを参照します：

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**サンプル出力：**

```
Alex
```

**データを追加または変更する：**

新しいペアを追加するか、既存のものを変更するには、キーを使用します：

```PowerShell
$myAssociativeArray["location"] = "New York" # 新しいキーと値のペアを追加
$myAssociativeArray["job"] = "シニアエンジニア" # 既存のペアを変更
```

**連想配列を繰り返し処理する：**

キーと値をこのようにループ処理します：

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**サンプル出力：**

```
name : Alex
age : 25
job : シニアエンジニア
location : New York
```

## 深掘り

連想配列の概念は多くのプログラミング言語で共通しており、通常は言語によって辞書、マップ、またはハッシュテーブルと呼ばれます。PowerShellでは、連想配列はハッシュテーブルとして実装されており、これはキーを検索したり、データを格納したり、一意のキーのコレクションを維持するのに非常に効率的です。

歴史的に、連想配列は、各アイテムをキーを使用して、コレクション全体を反復処理することなく迅速に取得できるように、オブジェクトのコレクションを管理する手段を提供します。データ取得と変更の効率性は、さまざまなタスクにおいて連想配列を好まれる選択肢とします。しかし、順序を維持するなどの制限がある場合、順序付き辞書やカスタムオブジェクトがより良い代替となる可能性があります。

その制限にもかかわらず、PowerShellでの連想配列/ハッシュテーブルは非常に柔軟で、スクリプティングにおいて強力なツールです。これらは動的なデータストレージを可能にし、構成、データ操作、および正式なクラス定義のオーバーヘッドなしに構造化されたデータフォーマットが必要な場所で特に有用です。連想配列はキーベースの取得には完璧ですが、タスクが複雑なデータ構造を含む場合や特定の順序を維持する必要がある場合は、PowerShell内の他のデータタイプやカスタムオブジェクトの探索を検討したいかもしれません。
