---
title:    "Python: 텍스트 파일 작성하기"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜 (Why) 
텍스트 파일 작성을 시작하기 전에 파이썬을 이용하여 텍스트 파일을 작성하는 이유에 대해 이해해야 합니다. 텍스트 파일은 컴퓨터에 저장된 데이터를 읽고 쓰기에 매우 효율적이며, 또한 특정한 형식에 맞춰 데이터를 구조화할 수 있습니다.

## 만드는 방법 (How To)
텍스트 파일을 만드는 가장 기본적인 방법은 `open()` 함수를 사용하는 것입니다. `open()` 함수는 파일 객체를 반환하며, 해당 파일을 열고 읽고 쓸 수 있는 권한을 가집니다. 아래는 파이썬을 이용하여 "hello.txt" 라는 이름의 텍스트 파일을 만드는 예시입니다.

```Python
file = open("hello.txt", "w")
file.write("Hello, world!")
file.close()
```

위 코드에서 `open()` 함수의 첫 번째 인자는 생성할 파일의 이름을, 두 번째 인자는 해당 파일을 어떤 모드로 열지를 나타냅니다. 위의 예시에서는 "w" 모드를 사용하여 파일을 쓰기 모드로 열었습니다. `write()` 메소드를 이용하여 파일에 데이터를 쓸 수 있습니다. 마지막으로 `close()` 메소드를 이용하여 파일을 닫아주면 됩니다.

위의 코드를 실행하면 "hello.txt" 파일이 생성되고 그 안에 "Hello, world!" 라는 문구가 저장될 것입니다.

## 깊이 파보기 (Deep Dive)
파이썬에서 파일을 작성하는 방식에는 여러 가지가 있습니다. 위에서 살펴본 `write()` 메소드 대신 `writelines()` 메소드를 사용하여 여러 줄의 데이터를 한 번에 파일에 쓸 수도 있습니다. 또한 `with` 구문을 이용하면 파일을 열고 닫는 과정을 자동으로 처리해줄 수 있습니다. 예시 코드는 아래와 같습니다.

```Python
with open("hello.txt", "w") as file:
    file.writelines(["Hello, world!", "Nice to meet you!"])
```

위 코드에서 `with` 구문은 코드 블록 안에서 파일을 열고 사용한 후 자동으로 닫아줍니다. 따로 `close()` 메소드를 사용하지 않아도 됩니다.

## 관련 자료 (See Also)
- [파이썬 입문서](https://wikidocs.net/book/1)
- [파이썬 기초 문법](https://dojang.io/)
- [Markdown 사용법](https://heropy.blog/2017/09/30/markdown/)