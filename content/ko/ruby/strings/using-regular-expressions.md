---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:18.653614-07:00
description: "\uB8E8\uBE44\uC5D0\uC11C\uC758 \uC815\uADDC\uC2DD(regex)\uC740 \uBB38\
  \uC790\uC5F4 \uB0B4\uC5D0\uC11C \uBB38\uC790 \uC870\uD569\uC744 \uB9E4\uCE58\uD558\
  \uAE30 \uC704\uD574 \uC0AC\uC6A9\uB418\uB294 \uD328\uD134\uC73C\uB85C, \uAC1C\uBC1C\
  \uC790\uB4E4\uC774 \uD14D\uC2A4\uD2B8\uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uAC80\
  \uC0C9, \uB9E4\uCE58, \uBC0F \uC870\uC791\uD560 \uC218 \uC788\uB3C4\uB85D \uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC720\uD6A8\uC131 \uAC80\
  \uC0AC, \uD30C\uC2F1, \uBB38\uC790\uC5F4 \uC870\uC791\uACFC \uAC19\uC740 \uC791\uC5C5\
  \uC744 \uC704\uD574 regex\uB97C \uD65C\uC6A9\uD558\uB294\uB370, \uC774\uB294 \uD14D\
  \uC2A4\uD2B8\u2026"
lastmod: '2024-03-13T22:44:55.977398-06:00'
model: gpt-4-0125-preview
summary: "\uB8E8\uBE44\uC5D0\uC11C\uC758 \uC815\uADDC\uC2DD(regex)\uC740 \uBB38\uC790\
  \uC5F4 \uB0B4\uC5D0\uC11C \uBB38\uC790 \uC870\uD569\uC744 \uB9E4\uCE58\uD558\uAE30\
  \ \uC704\uD574 \uC0AC\uC6A9\uB418\uB294 \uD328\uD134\uC73C\uB85C, \uAC1C\uBC1C\uC790\
  \uB4E4\uC774 \uD14D\uC2A4\uD2B8\uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uAC80\uC0C9\
  , \uB9E4\uCE58, \uBC0F \uC870\uC791\uD560 \uC218 \uC788\uB3C4\uB85D \uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC720\uD6A8\uC131 \uAC80\uC0AC\
  , \uD30C\uC2F1, \uBB38\uC790\uC5F4 \uC870\uC791\uACFC \uAC19\uC740 \uC791\uC5C5\uC744\
  \ \uC704\uD574 regex\uB97C \uD65C\uC6A9\uD558\uB294\uB370, \uC774\uB294 \uD14D\uC2A4\
  \uD2B8\u2026"
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용할까?
루비에서의 정규식(regex)은 문자열 내에서 문자 조합을 매치하기 위해 사용되는 패턴으로, 개발자들이 텍스트를 효율적으로 검색, 매치, 및 조작할 수 있도록 합니다. 프로그래머들은 유효성 검사, 파싱, 문자열 조작과 같은 작업을 위해 regex를 활용하는데, 이는 텍스트 처리에 있어 필수적인 도구입니다.

## 사용 방법:
### 기본 매칭
간단한 패턴에 대해 문자열을 매치하려면 `match` 메소드를 사용할 수 있습니다. 아래에서는 주어진 문자열 내에 "Ruby"라는 단어가 존재하는지 확인하고 있습니다.

```ruby
if /Ruby/.match("Hello, Ruby!")
  puts "매치 발견!"
end
# 출력: 매치 발견!
```

### 변수를 이용한 패턴 매칭
변수를 regex에 `#{}` 문법을 사용하여 내보내어 패턴을 동적으로 만들 수 있습니다.

```ruby
language = "Ruby"
if /#{language}/.match("Programming in Ruby is fun.")
  puts "Ruby에 대해 이야기 중!"
end
# 출력: Ruby에 대해 이야기 중!
```

### 정규식을 통한 치환
`gsub` 메소드를 사용하면 지정한 치환 문자열로 패턴의 모든 발생을 대체할 수 있습니다.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# 출력: barbarbar
```

### 캡쳐링
정규식에서 괄호는 매치의 일부를 캡쳐하는 데 사용됩니다. `match` 메소드는 캡쳐에 접근할 수 있도록 `MatchData` 객체를 반환합니다.

```ruby
match_data = /(\w+): (\d+)/.match("Age: 30")
puts match_data[1] # 캡쳐된 라벨
puts match_data[2] # 캡쳐된 값
# 출력:
# Age
# 30
```

### 타사 라이브러리 사용하기
루비의 표준 라이브러리가 강력하긴 하지만, 때로는 더 특화된 기능이 필요할 때가 있습니다. 정규식 작업에 있어 인기 있는 젬 중 하나는 `Oniguruma`로, 내장된 루비 정규식 엔진을 넘어서는 추가적인 정규식 기능을 제공합니다.

다음을 사용하여 설치하세요:
```bash
gem install oniguruma
```

설치한 후 `oniguruma`를 요구한 것을 가정하고 사용 예시는 다음과 같을 수 있습니다:

```ruby
# 이것은 좀 더 고급 예시일 수 있으며 추가 설정이 필요할 수 있습니다
require 'oniguruma'

pattern = Oniguruma::ORegexp.new('(\d+)')
match_data = pattern.match("The number is 42.")
puts match_data[1]
# 출력: 42
```

기억하세요, 정규식은 강력하긴 하지만, 더 복잡한 패턴에 대해서는 복잡하고 관리하기 어려울 수 있습니다. 가독성을 목표로 하고, 정규식이 너무 복잡해지면 대안적 방법을 고려하세요.
