---
title:                "Ruby: 텍스트 파일 읽기"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 읽는 것은 프로그래밍에서 매우 중요한 기술이다. 텍스트 파일을 읽는 방법을 배우면 다양한 파일 형식을 다룰 수 있고, 데이터를 처리하고 분석하는 데 도움이 된다.

## 어떻게

아래의 코드 예제를 통해 Ruby로 텍스트 파일을 읽는 방법을 알아보자.

```Ruby
# 파일 열기
file = File.open("sample.txt", "r")

# 파일 내용 읽기
contents = file.read

# 파일 닫기
file.close

# 내용 출력
puts contents
```

위의 코드는 "sample.txt" 파일을 읽고, 그 내용을 출력하는 간단한 예제이다.

## 딥 다이브

텍스트 파일을 읽는 방법은 간단하지만, 조금 더 깊은 수준에서 살펴보면 다양한 옵션이 있다. 예를 들어, 파일을 한 줄씩 읽을 수도 있고, 파일에 존재하는 특정 문자열을 찾을 수도 있다.

텍스트 파일을 읽는 과정에서 발생할 수 있는 오류도 잘 다루는 것이 중요하다. 오류 처리에 대한 더 자세한 정보는 다른 블로그 게시글을 참고하길 바란다.

## 관련 글

- [Ruby 파일 읽기와 쓰기](https://rubykr.github.io/ruby/corereference/file/)
- [RubyMine으로 텍스트 파일 읽기](https://blog.jetbrains.com/ruby/2012/10/rubymine-50-fle-editing/)
- [Ruby에서 파일 입출력](https://www.digitalocean.com/community/tutorials/how-to-work-with-files-using-ruby)