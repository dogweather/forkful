---
title:                "Ruby: 경로가 존재하는지 확인하는 방법"
simple_title:         "경로가 존재하는지 확인하는 방법"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

Ruby 는 디렉토리가 존재하는지 확인하는 기능을 제공하며, 이를 통해 코드 안전성을 높일 수 있습니다.

## 하는 방법

```Ruby
Dir.exist?('경로/디렉토리') #=> true or false
```

위의 예제 코드는 주어진 경로에 디렉토리가 존재하는지 확인하는 방법을 보여줍니다. 디렉토리가 존재하면 `true` 를 반환하고, 존재하지 않으면 `false` 를 반환합니다.

## 깊이 파고들기

디렉토리가 존재하는지 확인하기 위해서는 운영체제에게 `Dir.exist?` 메소드를 통해 해당 디렉토리 경로를 전달해줘야 합니다. 또한 디렉토리의 이름이나 경로가 정확하게 입력되어야 하며, 디렉토리가 존재하지 않는 경우 올바른 오류 처리를 해주어야 합니다.

## 또 다른 방법 알아보기

- [Ruby 표준 라이브러리 문서 - Dir](https://ruby-doc.org/core-2.6.3/Dir.html)
- [Ruby 가이드 - 디렉토리 다루기](https://www.rubyguides.com/2018/10/ruby-dir-glob/)
- [rubyDir.exist?의 장단점](https://luizaugustomm.me/blog/rubydir-exist)