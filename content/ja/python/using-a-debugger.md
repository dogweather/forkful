---
title:                "デバッガーの使い方"
date:                  2024-01-26T04:09:15.587095-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
「デバッガの使用」とは、Pythonコードをステップ実行してバグを発見し、動作を理解することです。これを行う理由は、単に問題がどこで発生したかを推測するよりもはるかに簡単だからであり、私たちを何時間ものprint文の煉獄から救ってくれます。

## 使い方：
Pythonの組み込みデバッガである`pdb`の使用方法を見ていきましょう。`buggy.py`というファイルがあり、それには見つけにくいバグがありますと想像してください：

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

このスクリプトを実行すると、`8`が期待されますが、ただの構文エラーを投げます。デバッガの時間です！

ターミナルで実行してください：
```bash
python -m pdb buggy.py
```

デバッガに入ると、次のように表示されます：
```Python
> /path_to_file/buggy.py(1)<module>()
-> def add_one(number):
```

`l(ist)`でより多くのコードを見る、`n(ext)`で次の行に進む、または`c(ontinue)`でスクリプトを実行続けることができます。エラーに当たったとき、`pdb`は停止して検査させてくれます。

`number ++ 1`を`number + 1`に訂正した後、デバッガを再起動して修正をテストしてください。
忘れずに、友達は友達をネットなしでコードさせません。これで十分です。

## 深堀り
プログラミングの暗黒時代（つまり、統合開発環境（IDE）が至る所にある前）には、デバッガはしばしばテキストエディタの外で使用するスタンドアローンのツールでした。これらは、様々な実行ポイントでソフトウェアの状態を検査できるようにして、プログラマーを救済しました。

2023年時点で、Pythonの`pdb`は唯一の選択肢ではありません。PyCharmやVisual Studio CodeのようなIDEを使用する人もいて、これらはクリック一つで設定できるブレークポイントのような便利な機能を備えた自前の洗練されたデバッガを組み込んでいます。

そして、`ipdb`があります。これは`IPython`の良さをデバッグにもたらすpipでインストール可能なパッケージです。タブ補完や構文ハイライトを備えた、`pdb`のパフォーマンスエンハンサーのようなものです。

デバッガはその実装においても異なります。一部は、マシンレベルまたはバイトコードレベルでのプログラム実行に密接に関与します。他の多くの高レベル言語のデバッガのように、変数の状態を監視し、実行フローを制御する特別な環境でコードを実行するものもあります。

## 参照
Pythonのデバッガ全般について詳しくは、以下をご覧ください：
- `pdb`ドキュメント：https://docs.python.org/3/library/pdb.html

代替手段に興味がある場合は、これらのリンクが役立ちます：
- `ipdb`リポジトリと使用ガイド：https://github.com/gotcha/ipdb
- Visual Studio Codeによるデバッグ：https://code.visualstudio.com/docs/python/debugging
- PyCharmのデバッグ機能：https://www.jetbrains.com/help/pycharm/debugging-code.html

楽しいバグ探しを！