---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 쓰기는 데이터를 영구적으로 저장하는 과정입니다. 프로그래머들은 설정, 로그, 데이터 교환 같은 목적으로 이를 활용합니다.

## How to: (어떻게:)
```Ruby
# 기본적인 텍스트 파일 쓰기
File.open("example.txt", "w") do |file|
  file.puts "안녕하세요, Ruby 사용자!"
end

# 파일 내용 읽기
puts File.read("example.txt")
```
출력:
```
안녕하세요, Ruby 사용자!
```

```Ruby
# 여러줄 추가하기
File.open("example.txt", "a") do |file|
  file.puts "파일에 추가된 내용입니다."
  file.puts "또 추가했습니다."
end
```
출력:
```
안녕하세요, Ruby 사용자!
파일에 추가된 내용입니다.
또 추가했습니다.
```

## Deep Dive (심층 분석)
과거에는 데이터를 파일에 저장하기 위해 저수준 언어를 사용하는 경우가 많았지만, Ruby 같은 현대 언어는 직관적인 API를 제공합니다. `IO` 클래스가 이 기능의 핵심이며 `File` 클래스는 `IO`를 상속받아 파일 입출력을 쉽게 합니다. `write`, `puts`, `print` 메소드를 사용해 다양한 방식으로 데이터를 파일에 쓸 수 있고, `"w"`, `"a"` 등 다양한 옵션으로 파일 모드를 설정할 수 있습니다.

## See Also (참고자료)
- Ruby 문서의 IO 클래스: [https://ruby-doc.org/core-3.1.0/IO.html](https://ruby-doc.org/core-3.1.0/IO.html)
- Ruby 문서의 File 클래스: [https://ruby-doc.org/core-3.1.0/File.html](https://ruby-doc.org/core-3.1.0/File.html)
- 파일 입출력에 관한 Ruby 스타일 가이드: [https://rubystyle.guide/#working-with-files](https://rubystyle.guide/#working-with-files)