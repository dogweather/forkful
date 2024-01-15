---
title:                "텍스트 파일 읽기"
html_title:           "Elixir: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍은 현대 사회에서 매우 중요합니다. 따라서 한국인 독자분께서는 다양한 프로그래밍 언어를 배우고 싶어할 것입니다. 이번 글에서는 최신 버전인 엘릭서 프로그래밍 언어를 소개하도록 하겠습니다. 엘릭서는 함수형 언어이면서 동시에 강력한 도구를 제공하므로 프로그램 개발에 매우 적합합니다.

## 어떻게

우선, 엘릭서에서 텍스트 파일을 읽는 방법을 알아보겠습니다. 아래는 간단한 예제 코드입니다.

```elixir
File.read("sample.txt") # 텍스트 파일 읽기
|> String.split("\n") # 한줄씩 분리
|> Enum.map(fn line -> line |> String.trim() |> String.upcase() end) # 각 줄을 공백을 제거하고 대문자로 변환
|> Enum.each(fn line -> IO.inspect(line) end) # 결과 출력
```

위 코드를 실행하면 텍스트 파일의 내용을 한 줄씩 읽어서 공백을 제거하고 대문자로 변환한 뒤, 각 줄을 출력합니다. 즉, 엘릭서에서는 간단한 코드만으로도 텍스트 파일을 쉽게 읽을 수 있습니다.

또한 엘릭서는 파일을 읽는 다양한 함수들을 제공합니다. 예를 들어 `File.read!`는 파일을 읽는 도중 오류가 발생하면 프로그램을 중단시키지만, `File.read`는 오류가 발생해도 계속해서 프로그램을 진행합니다. 이처럼 엘릭서는 프로그래머가 원하는 방식으로 파일을 읽을 수 있도록 다양한 옵션을 제공합니다.

## 심화 학습

이제 텍스트 파일을 읽는 방법을 세부적으로 살펴보겠습니다. 엘릭서에서 파일을 읽을 때는 `File.read` 함수를 사용합니다. 이 함수는 파라미터로 파일의 경로를 받아서 해당 파일의 내용을 읽어옵니다. 그리고 `String.split` 함수를 사용하면 읽어온 파일의 내용을 원하는 형태로 분리할 수 있습니다. 예를 들어 첫번째 예제에서는 파일을 한 줄씩 분리하고 각 줄을 공백을 제거하고 대문자로 변환하는 방식으로 파일을 읽었습니다.

## 참고

- [엘릭서 공식 문서(한글)](https://elixir-lang.org/getting-started/introduction.html)
- [File 모듈(한글)](https://elixir-lang.org/getting-started/introduction.html#mix-and-the-projects-it-generates)