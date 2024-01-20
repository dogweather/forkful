---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 파일을 읽는 것은 원시 텍스트 데이터를 컴퓨터로 가져와서 읽을 수 있게 하는 것입니다. 프로그래머들이 이것을 하는 이유는 파일 내용을 분석하고 필요한 정보를 추출하기 위해서입니다.

## 어떻게하나요:

작은 예로부터 시작하겠습니다. 먼저, 우리는 `read` 메소드를 사용하여 텍스트 파일을 읽을 수 있습니다.

```Ruby
contents = File.read("sample.txt")
puts contents
```

위의 코드는 `sample.txt`라는 텍스트 파일의 모든 내용을 읽고 화면에 출력합니다.

## Deep Dive

이 기능은 Ruby 프로그래밍언어의 초기버전에서부터 제공되었으며, 파일을 다루는 가장 기본적인 기능 중 하나입니다. Ruby에서 파일을 읽는 대안은 다음과 같습니다:

- `readlines` 메소드를 사용하여 파일의 각 행을 배열로 읽을 수 있습니다.

```Ruby
lines = File.readlines("sample.txt")
puts lines
```

- `each_line` 메소드를 사용하여 파일의 각 행을 순차적으로 읽을 수 있습니다.

```Ruby
File.each_line("sample.txt") do |line|
  puts line
end
```

Ruby에서 텍스트 파일을 읽는 구현의 핵심은 `IO` 클래스입니다. 이 클래스는 모든 입출력 작업에 대한 메소드를 제공하는데, `read`, `readlines`, `each_line` 등이 여기에 속합니다.

## 추가정보

다음의 링크에서 관련 정보를 더 찾아볼 수 있습니다:
- Ruby에서 파일 다루기: [https://ruby-doc.org/core-2.7.1/IO.html](https://ruby-doc.org/core-2.7.1/IO.html)
- Ruby 입출력 튜토리얼: [https://www.tutorialspoint.com/ruby/ruby_input_output.htm](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)