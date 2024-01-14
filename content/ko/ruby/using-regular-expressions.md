---
title:    "Ruby: 정규 표현식을 사용하기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 특정 패턴을 검색하고, 찾은 패턴을 대체하거나 수정하고, 데이터를 정제하는 것이 더 쉽기 때문입니다.

## 사용 방법

```Ruby
# 지정된 단어가 있는지 확인
str = "안녕하세요, 반가워요."
pattern = /반가워/
puts str.match(pattern) ? "찾았어요!" : "못찾음 :("

# 문자열에서 모든 모음을 제거
str = "학교에서 배우는 프로그래밍은 재미있어요."
pattern = /[aeiou]/
puts str.gsub(pattern, "") # 학교에서 배우는 프로그래밍은 재미있어요.

# 특정 패턴이 포함된 파일 이름 출력
files = ["image1.jpg", "textfile.txt", "script.rb", "backup.zip"]
pattern = /[aeiou]/
files.each do |file|
  puts file if pattern.match(file)
end
# image1.jpg
# textfile.txt

# 이메일 유효성 검사
def valid_email?(email)
  pattern = /\A[\w+\-.]+@[a-z\d\-]+(\.[a-z\d\-]+)*\.[a-z]+\z/i 
  # @ 이후의 도메인 주소에는 최소 1개의 .이 있어야 함 (옵션이지만 반드시 필요한 것은 아님)
  email.match(pattern) ? true : false
end

puts valid_email?("example123@example.com") # true
puts valid_email?("invalid@com") # false
```

## 깊이 파고들기

정규 표현식을 사용하면 문자열을 처리하는 방법이 무궁무진해집니다. 예를 들어, 백엔드 웹 애플리케이션을 개발하고 있다고 가정해 봅시다. 사용자의 입력 폼에서 받은 전화번호를 저장할 때 숫자만 저장하고 싶다면 어떻게 할까요? 이 때 정규 표현식은 아주 유용합니다. 전화번호 입력 폼에는 10자리 숫자를 입력해야 하므로, `/[0-9]{10}/` 과 같은 패턴을 사용하여 숫자만 추출할 수 있습니다.

## 다른 자료들

- [레일스 초보자를 위한 정규 표현식 가이드](https://poignant.guide/book/chapter-3.html)
- [Ruby Regex Cheatsheet](https://www.shortcutfoo.com/app/dojos/ruby-regex/cheatsheet)
- [루비 정규 표현식 튜토리얼](https://www.rubyguides.com/2015/06/ruby-regex/)
- [루비 프로그래밍 언어 공식 문서](https://ruby-doc.org/core-2.7.1/doc/regexp_rdoc.html#label-Character+Classes)