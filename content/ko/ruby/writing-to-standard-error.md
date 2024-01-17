---
title:                "표준 에러에 쓰는 방법"
html_title:           "Ruby: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

"표준 오류에 쓰기"는 프로그래밍에서 오류 메시지를 표준 오류 스트림에 출력하는 작업을 의미합니다. 프로그래머들은 오류 메시지를 표준 오류 스트림에 쓰는 이유는 예상치 못한 오류가 발생하거나 디버깅을 도와주기 위해서입니다.

## 사용 방법:

```Ruby
# 예제 1: 표준 오류에 문자열 출력하기
STDERR.puts "오류가 발생했습니다!"

# 예제 2: 파일을 열 때 발생하는 오류 출력하기
begin
  File.open("test.txt", "r")
rescue
  STDERR.puts "파일을 열 수 없습니다!"
end
```

### 출력 예:

```
오류가 발생했습니다!
파일을 열 수 없습니다!
```

## 깊게 파보기:

### 역사적 배경:

표준 오류 스트립에 쓰기는 오래 전부터 사용되어온 기술입니다. 이는 오류를 콘솔에 축력하여 디버깅을 도와준다는 장점이 있었기 때문입니다.

### 대체 방법:

오류 메시지를 쓰기 위해서는 모든 프로그래밍 언어에서 사용할 수 있는 `STDERR` 객체를 이용하면 됩니다. 하지만, 에러 처리에 특화된 라이브러리들을 사용한다면 더 나은 오류 처리가 가능합니다.

### 구현 세부 사항:

표준 오류 스트립에 쓰는 방법은 간단합니다. `STDERR` 객체의 `puts` 메소드를 이용하여 오류 메시지를 출력하면 됩니다. 또한, `STDERR` 객체의 `write` 메소드를 이용하면 더 적은 버퍼링을 거치고 빠른 출력이 가능합니다.

## 더 알아보기:

- Ruby의 `STDERR` 문서: https://ruby-doc.org/core/IO.html#method-i-puts
- 오류 처리에 대한 Best Practices: https://www.toptal.com/ruby-on-rails/top-10-mistakes-that-rails-programmers-make (영문)