---
title:                "텍스트 파일 읽기"
html_title:           "Ruby: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
텍스트 파일을 읽는 것은 프로그래머가 텍스트 데이터를 읽고 처리하는 것을 의미합니다. 이 작업은 데이터를 분석하거나 가공할 때 종종 필요한 기본적인 작업입니다.

## 어떻게:
```Ruby
# 파일 읽기 모드로 파일 열기
file = File.open("example.txt", "r")

# 파일 내용 읽어오기
file_data = file.read

# 파일 닫기
file.close

# 읽어온 내용 출력
puts file_data
```

위의 코드 예제는 "example.txt"라는 이름의 파일을 읽어 그 내용을 출력하는 간단한 방법을 보여줍니다.

## 깊이 파싱:
텍스트 파일을 읽는 작업은 프로그래밍의 기본적인 부분 중 하나입니다. 코드에서 데이터를 입력받는 방법을 배우게 되면 파일을 읽는 작업도 필연적으로 배우게 됩니다.

텍스트 파일을 읽는 작업에는 여러 가지 대안이 존재합니다. 예를 들어 위의 예제에서는 `File` 클래스를 이용하였지만 `IO` 모듈을 이용하는 방법도 있습니다. 그리고 파일을 한 번에 전체적으로 읽는 대신 한 줄씩 읽는 방법도 존재합니다.

파일을 읽을 때는 파일이 항상 존재한다는 것을 가정하고 사용하는 것이 좋습니다. 그리고 만약 예상치 못한 오류가 발생할 수 있는 경우 `begin-rescue-end` 블록을 사용하여 예외를 처리하는 것이 좋습니다.

## 참고자료:
- [Ruby 파일 IO](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [IO 모듈](https://ruby-doc.org/core/IO.html)
- [Ruby 예외 처리](https://www.ruby-lang.org/en/documentation/exceptions/)

이 문서는 Ruby의 `File` 클래스를 이용해 파일을 읽는 방법을 간단하게 소개하고 있습니다. 더 깊이 파고들고 싶다면 위의 참고자료를 참고하여 더 많은 정보를 얻을 수 있습니다.