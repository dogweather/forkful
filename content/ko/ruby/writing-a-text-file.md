---
title:                "Ruby: 텍스트 파일 작성하기"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 쓰는 이유는 우리가 작성한 코드나 데이터를 나중에 쉽게 찾고 사용할 수 있도록 저장하기 위해서입니다.

## 어떻게

텍스트 파일을 쓰려면 먼저 Ruby의 `File` 클래스를 사용해 파일을 생성해야 합니다. 그리고 `File.open` 메소드를 사용해 파일을 열고 `write` 메소드로 내용을 작성합니다. 마지막으로 `close` 메소드로 파일을 닫아줍니다.

```Ruby
file = File.new("my_file.txt", "w")
file.write("Hello, world!") # 파일 내용 작성
file.close # 파일 닫기
```

## 더 깊게 알아보기

Ruby에서 텍스트 파일을 쓰는 것은 매우 간단하지만 몇 가지 주의할 점이 있습니다. 우선 쓰기 모드인 `"w"`를 사용할 때 파일이 존재하지 않으면 새로 생성되지만 이미 존재하는 파일이 있다면 기존 내용이 삭제됩니다. 또한 파일의 디렉토리 위치는 `File.expand_path` 메소드를 사용해 현재 디렉토리를 기준으로 설정할 수 있습니다.

## 더 알아보기

- [Ruby 의 File 클래스 문서](https://ruby-doc.org/core-3.0.0/File.html)
- [Ruby 의 내장 함수를 사용해 파일 쓰기](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [텍스트 파일을 쓰고 읽는 방법](https://www.rubyguides.com/2015/05/working-with-files-ruby/)

# 더 알아보기

- [Ruby 의 텍스트 파일 다루기](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Ruby 와 파일 입출력에 대한 자세한 설명](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Ruby 에서 CSV 파일 다루기](https://www.rubyguides.com/2018/10/parse-csv-ruby/)

# 관련 자료

- [Ruby 의 기초 문법](https://learnrubythehardway.org/book/index.html)
- [Ruby 의 표준 라이브러리 문서](https://ruby-doc.org/stdlib-2.7.2/)
- [Ruby 와 Rails 에 대한 무료 온라인 강좌](https://www.udemy.com/topic/ruby-on-rails/)