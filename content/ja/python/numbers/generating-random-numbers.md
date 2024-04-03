---
date: 2024-01-27 20:35:24.495195-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Python\u306F\u3001\u3055\
  \u307E\u3056\u307E\u306A\u7528\u9014\u306E\u305F\u3081\u306E\u4E71\u6570\u3092\u751F\
  \u6210\u3059\u308B\u306E\u306B\u5F79\u7ACB\u3064`random`\u30E2\u30B8\u30E5\u30FC\
  \u30EB\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u65B9\
  \u6CD5\u3067\u59CB\u3081\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\uFF1A 1.\
  \ **\u30E2\u30B8\u30E5\u30FC\u30EB\u306E\u30A4\u30F3\u30DD\u30FC\u30C8**."
lastmod: '2024-03-13T22:44:41.494792-06:00'
model: gpt-4-0125-preview
summary: "Python\u306F\u3001\u3055\u307E\u3056\u307E\u306A\u7528\u9014\u306E\u305F\
  \u3081\u306E\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u306E\u306B\u5F79\u7ACB\u3064\
  `random`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\
  \u3059\u3002\u4EE5\u4E0B\u306E\u65B9\u6CD5\u3067\u59CB\u3081\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\uFF1A\n\n1."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

## どのようにして：
Pythonは、さまざまな用途のための乱数を生成するのに役立つ`random`モジュールを提供しています。以下の方法で始めることができます：

1. **モジュールのインポート**
    ```Python
    import random
    ```

2. **ランダムな整数の生成**
    任意の二つの数の間で。
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    サンプル出力: `7`

3. **浮動小数点数の生成**
    0と1の間で。
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    サンプル出力: `0.436432634653`

    異なる範囲で浮動小数点数が必要な場合、乗算します：
    ```Python
    random_float_range = random.random() * 5  # 0から5まで
    print(random_float_range)
    ```
    サンプル出力: `3.182093745`

4. **リストからランダムな要素を選択する**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    サンプル出力: `Hola`

5. **リストをシャッフルする**
    カードゲームや順序をランダム化する必要があるあらゆるアプリケーションに最適です。
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    サンプル出力: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## 詳細な解説
Pythonの`random`モジュールは擬似乱数生成器（PRNG）を使用しており、具体的にはメルセンヌ・ツイスタアルゴリズムを使用しています。これは一般的なアプリケーションには適していますが、十分な出力を観測することで予測可能であるため、暗号学的な目的には不向きです。Python 3.6で導入された`secrets`モジュールは、セキュリティに敏感なアプリケーションで暗号学的に強固な乱数を生成するためのより良い代替手段を提供します。例えば、パスワードリセットリンクのための安全なランダムトークンを生成する場合：

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

歴史的に、真にランダムな数を生成することは、物理現象や手動で入力されたシードに依存する初期の方法を含め、コンピューティングにおいて挑戦でした。メルセンヌ・ツイスタ（少なくとも2023年の私の最後の知識更新時点でPythonの`random`モジュールでデフォルトで使用されている）のようなアルゴリズムの開発と採用は、顕著な進歩を示しました。しかし、より安全で効率的なアルゴリズムを求める継続的な探求は、暗号関連のタスクのために`secrets`モジュールを含むに至りました。この進化は、ソフトウェア開発におけるセキュリティの重要性の高まりと、暗号化から安全なトークン生成に至るまでのアプリケーションにおけるより強固な乱数の必要性を反映しています。
