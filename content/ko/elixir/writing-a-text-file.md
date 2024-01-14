---
title:                "Elixir: 텍스트 파일 쓰기"
simple_title:         "텍스트 파일 쓰기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것의 이유는 가독성과 코드 재사용성을 높이기 위해서입니다.

## 어떻게

다음은 Elixir에서 텍스트 파일을 작성하는 방법의 예시 코드와 출력입니다.

```Elixir
# 파일을 생성하고 텍스트를 쓸 파일 객체를 반환합니다.
{:ok, file} = File.open("example.txt", [:write])

# 파일에 텍스트를 씁니다.
IO.write(file, "안녕하세요!")

# 파일을 닫습니다.
File.close(file)
```

출력:

> example.txt
> "안녕하세요!"

## 심층 분석

텍스트 파일을 작성할 때 유의해야 할 몇 가지 유용한 팁이 있습니다. 

첫째, 파일을 열 때 `:write` 옵션을 제공하면, 파일이 존재하지 않을 경우 생성이 됩니다. 또한 이미 존재하는 파일일 경우 덮어쓰기가 됩니다.

둘째, `IO.write/2`는 문자열 외에도 모든 데이터 타입을 받을 수 있습니다. 그래서, `IO.inspect/2`나 다른 함수에서 받은 결과를 쉽게 파일에 출력할 수 있습니다.

셋째, `File.close/1`를 꼭 호출해줘야 합니다. 이는 파일에 대한 권한을 돌려주는 역할을 합니다.

마지막으로, Elixir에서는 `IO.ostream_data/1`함수를 사용해 파일 객체에서 데이터를 읽을 수 있습니다. 이를 통해 데이터를 처리하거나, 다른 파일에 쓸 수도 있습니다.

## 참고

[File 모듈 문서](https://hexdocs.pm/elixir/File.html)
[IO 모듈 문서](https://hexdocs.pm/elixir/IO.html)
[Markdown 문서](https://daringfireball.net/projects/markdown/syntax)