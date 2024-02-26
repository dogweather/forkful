---
date: 2024-01-26 00:56:03.209646-07:00
description: "PowerShell\u3067\u306E\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u4E88\
  \u671F\u305B\u306C\u30C8\u30E9\u30D6\u30EB\u3092\u4E88\u6E2C\u3057\u3066\u3001\u30B9\
  \u30E0\u30FC\u30BA\u306B\u7BA1\u7406\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\
  \u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\u3001\u30E6\u30FC\u30B6\u306B\
  \u6709\u7528\u306A\u30D5\u30A3\u30FC\u30C9\u30D0\u30C3\u30AF\u3092\u63D0\u4F9B\u3057\
  \u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.419076-07:00'
model: gpt-4-1106-preview
summary: "PowerShell\u3067\u306E\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u4E88\u671F\
  \u305B\u306C\u30C8\u30E9\u30D6\u30EB\u3092\u4E88\u6E2C\u3057\u3066\u3001\u30B9\u30E0\
  \u30FC\u30BA\u306B\u7BA1\u7406\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\
  \u30AF\u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\u3001\u30E6\u30FC\u30B6\u306B\u6709\
  \u7528\u306A\u30D5\u30A3\u30FC\u30C9\u30D0\u30C3\u30AF\u3092\u63D0\u4F9B\u3057\u307E\
  \u3059\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？
PowerShellでのエラー処理は、予期せぬトラブルを予測して、スムーズに管理することです。プログラマーはこれを行うことで、クラッシュを防ぎ、ユーザに有用なフィードバックを提供します。

## 方法：
```PowerShell
# 例外を処理する基本的なTry-Catch
try {
    # エラーを引き起こす可能性があるコード
    $result = 1 / 0
} catch {
    # エラーが発生した場合の対処方法
    Write-Host "おっと、エラーが発生しました: $_"
}

# カスタムエラーメッセージの出力
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "ファイルが見つかりませんでした。"
}

# 最後のエラーを調べるための$Error変数の使用
```
## 深く掘り下げて
PowerShellはMonadとして初めて登場して以来、長い道のりを歩んできました。エラー処理は時間と共により堅牢になり、C#のような他のプログラミング言語と似た機能を提供するようになりました。`try-catch-finally`構文はそのような他の言語からの影響を受けたものの一つです。それ以前には、スクリプターは条件のチェックや`$Error`自動変数の使用に大きく依存していました。

PowerShellには、終了エラーと非終了エラーという2つの主要なエラータイプがあります。終了エラーは`try-catch`ブロックで捕捉されない限りスクリプトを停止させますが、非終了エラーは`-ErrorAction Stop`を指定しない限り停止させません。この区別は、エラーがスクリプト全体を停止するほどのものか、単に記録して無視できるものかを決める上で不可欠です。

PowerShellのエラー処理では、`finally`ブロックも利用できます。これは何が起ころうとも-エラーが発生したかどうかに関わらず-実行されます。クリーンアップタスクに最適です。

スクリプトを深く掘り下げるときには、さらに細かい制御が可能となり、特定の例外タイプを扱うことができます。

また、古い方法として、例外をスローすることなくエラーをキャプチャするための`-ErrorVariable`パラメータや、最後の操作が成功したかどうかを教えてくれる`$?`変数もあります。彼らは便利なツールですが、しっかりした`try-catch`に比べれば少し整理されていない感じがします。

## 参照
- [about_Try_Catch_Finally](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
