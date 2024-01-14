---
title:    "Elixir: 컴퓨터 프로그래밍을 위한 텍스트 파일 쓰기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?

텍스트 파일의 작성은 프로그래밍에서 가장 기초적이면서도 중요한 요소 중 하나입니다. 텍스트 파일은 코드의 저장과 공유를 쉽게 만들어주며, 버전 관리 시스템과의 상호 작용 역시 용이해집니다. 따라서, 프로그래머로서 텍스트 파일 작성을 배우는 것은 매우 중요합니다.

## 어떻게?

텍스트 파일을 만드는 법은 간단합니다. Elixir에서는 `File.write/2` 함수를 사용하여 새로운 파일을 생성하고, 내용을 작성할 수 있습니다. 예를 들어, 다음과 같은 코드로 `hello.txt` 파일을 생성하고 내용을 작성할 수 있습니다.

```elixir
File.write("hello.txt", "안녕하세요!")
```
`"hello.txt"`는 생성할 파일의 이름을 나타내며, `"안녕하세요!"`는 해당 파일에 작성할 내용입니다. 위 코드를 실행하면 현재 디렉토리에 `hello.txt` 파일이 생성되고, 해당 파일 내용을 열어보면 `"안녕하세요!"`가 작성되어 있는 것을 확인할 수 있습니다.

## 깊이 파고들기

텍스트 파일을 작성할 때 더욱 세부적인 조작을 하고 싶다면, Elixir의 `File` 모듈에서 제공하는 다양한 함수들을 이용할 수 있습니다. 예를 들어, 다음 코드를 통해 이전에 생성했던 `hello.txt` 파일에 내용을 추가할 수 있습니다.

```elixir
File.append("hello.txt", "또 만나요!")
```

위 코드를 실행하면 `hello.txt` 파일에 `"안녕하세요!또 만나요!"`가 작성되는 것을 확인할 수 있습니다.

## 또 다른 정보들

텍스트 파일을 생성하고 조작하는 종류의 작업들은 무궁무진합니다. 따라서, 더 많은 정보를 얻고 싶다면 Elixir 공식 문서를 참조해보는 것을 추천합니다.

## 참고

- [Elixir File 모듈 문서](https://hexdocs.pm/elixir/File.html)
- [Elixir 공식 문서](https://elixir-lang.org/docs.html)