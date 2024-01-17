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

## 무엇 & 왜?

텍스트 파일을 작성하는 것은 간단히 말하면 컴퓨터에 저장되어 있는 텍스트 정보를 문서로 만드는 것입니다. 프로그래머들은 텍스트 파일을 작성하는 이유는 여러가지가 있지만, 가장 일반적인 이유는 데이터를 저장하고 나중에 불러오기 위해서입니다.

## 방법:

```ruby
# 새로운 텍스트 파일 생성
File.write("새로운파일.txt", "안녕하세요!")

# 기존의 텍스트 파일에 내용 추가하기
File.open("기존파일.txt", 'a') do |file|
  file.write("또 다른 내용을 추가합니다.")
end

# 텍스트 파일 읽기
File.open("파일.txt", "r") do |file|
  puts file.read
end
```

**출력:**
```
안녕하세요!
또 다른 내용을 추가합니다.
```

## 깊이 파고들기:

- 텍스트 파일은 컴퓨터의 파일 시스템에 저장되어 있으며, 일반적으로 텍스트 에디터로 열 수 있습니다.
- 텍스트 파일을 작성하는 다른 방법으로는 터미널을 이용하는 것이 있습니다. 예를 들어, `echo "추가할 내용" > 파일.txt` 명령어를 사용할 수 있습니다.
- 텍스트 파일의 포멧은 여러가지가 있지만, 가장 일반적인 것은 ASCII와 UTF-8입니다.

## 관련 정보 보기:

- [Ruby 파일 입출력 가이드](https://rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/48-ruby-on-your-system/lessons/122-file-i-o-in-your-system)
- [텍스트 파일 개념 이해하기](https://www.computerhope.com/jargon/t/textfile.htm)
- [텍스트 파일 작성에 관련된 더 많은 예제](https://gist.github.com/codertimo/829280)