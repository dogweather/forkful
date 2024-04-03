---
date: 2024-01-26 04:09:15.587095-07:00
description: "Python\u306E\u7D44\u307F\u8FBC\u307F\u30C7\u30D0\u30C3\u30AC\u3067\u3042\
  \u308B`pdb`\u306E\u4F7F\u7528\u65B9\u6CD5\u3092\u898B\u3066\u3044\u304D\u307E\u3057\
  \u3087\u3046\u3002`buggy.py`\u3068\u3044\u3046\u30D5\u30A1\u30A4\u30EB\u304C\u3042\
  \u308A\u3001\u305D\u308C\u306B\u306F\u898B\u3064\u3051\u306B\u304F\u3044\u30D0\u30B0\
  \u304C\u3042\u308A\u307E\u3059\u3068\u60F3\u50CF\u3057\u3066\u304F\u3060\u3055\u3044\
  \uFF1A ```Python def add_one(number): result = number ++ 1 return result\u2026"
lastmod: '2024-03-13T22:44:41.505661-06:00'
model: gpt-4-0125-preview
summary: "Python\u306E\u7D44\u307F\u8FBC\u307F\u30C7\u30D0\u30C3\u30AC\u3067\u3042\
  \u308B`pdb`\u306E\u4F7F\u7528\u65B9\u6CD5\u3092\u898B\u3066\u3044\u304D\u307E\u3057\
  \u3087\u3046\u3002`buggy."
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

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
