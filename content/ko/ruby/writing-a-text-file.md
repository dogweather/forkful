---
title:                "텍스트 파일 작성하기"
html_title:           "Ruby: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?

누군가 Ruby를 사용하여 텍스트 파일을 작성할 이유는 무엇일까요? 사실 많은 이유가 있을 수 있지만, 가장 큰 이유는 텍스트 파일을 사용하여 자신의 코드나 데이터를 저장하고 관리하는 데 있습니다.

## 하우 투

아래는 Ruby를 사용하여 텍스트 파일을 작성하는 간단한 예제입니다.

```Ruby
# 텍스트 파일을 쓰기 모드로 열기
File.open("my_text_file.txt", "w") do |file|
    # 파일에 내용 작성
    file.puts "안녕하세요, 여러분!"
    file.puts "Ruby로 텍스트 파일을 작성하는 방법을 알아봅시다."
end
```

위 코드를 실행하면 "my_text_file.txt"라는 이름의 텍스트 파일이 생성되고, 그 안에는 "안녕하세요, 여러분!"과 "Ruby로 텍스트 파일을 작성하는 방법을 알아봅시다."라는 두 줄의 텍스트가 저장됩니다.

## 딥 다이브

텍스트 파일을 작성하는 방법은 간단하지만, 실제로는 다양한 옵션이 존재합니다. 예를 들어, `File.open` 메소드의 두 번째 파라미터로 "w"를 사용하였지만, 다른 옵션으로 "a" 또는 "r"을 사용할 수도 있습니다. 또한 파일을 열고 닫을 때 `File.open` 메소드 대신 `File.open` 블록을 사용하면 더욱 안전하게 파일을 다룰 수 있습니다.

## 더 보기

- [Ruby 문서: 파일 다루기](https://ruby-doc.org/core-3.0.0/File.html)
- [Ruby Monstas: 파일 다루기](https://rubymonstas.org/ko/topics/files.html)
- [유용한 Ruby 비디오 강좌: 텍스트 파일 다루기](https://www.youtube.com/watch?v=ZTI5Fsv6_cM)