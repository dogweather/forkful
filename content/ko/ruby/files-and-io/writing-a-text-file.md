---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:07.828150-07:00
description: "\uBC29\uBC95: Ruby\uB294 \uD30C\uC77C \uC791\uC5C5\uC744 \uAC04\uB2E8\
  \uD558\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4. \uD30C\uC77C\uC5D0 \uC4F0\uAE30 \uC704\uD574\
  , Ruby\uC5D0 \uB0B4\uC7A5\uB41C `File` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uB2E4\uC74C \uC608\uC81C\uB294 \uD30C\uC77C\uC744\
  \ \uC4F0\uAE30 \uBAA8\uB4DC(`\"w\"` \uBAA8\uB4DC)\uC640 \uCD94\uAC00 \uBAA8\uB4DC\
  (`\"a\"` \uBAA8\uB4DC)\uB85C \uC5F4\uACE0, \uBB38\uC790\uC5F4\uC744 \uC4F4 \uD6C4\
  , \uD30C\uC77C\uC774 \uC774\uD6C4\uC5D0 \uB2EB\uD788\uB294 \uAC83\uC744 \uBCF4\uC7A5\
  \uD558\uB294\u2026"
lastmod: '2024-03-13T22:44:56.022687-06:00'
model: gpt-4-0125-preview
summary: "Ruby\uB294 \uD30C\uC77C \uC791\uC5C5\uC744 \uAC04\uB2E8\uD558\uAC8C \uB9CC\
  \uB4ED\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:
Ruby는 파일 작업을 간단하게 만듭니다. 파일에 쓰기 위해, Ruby에 내장된 `File` 클래스를 사용할 수 있습니다. 다음 예제는 파일을 쓰기 모드(`"w"` 모드)와 추가 모드(`"a"` 모드)로 열고, 문자열을 쓴 후, 파일이 이후에 닫히는 것을 보장하는 방법을 보여줍니다:

```ruby
# 새로운 내용을 파일에 쓰기, 기존 내용을 덮어쓰기
File.open("example.txt", "w") do |file|
  file.puts "Hello, Ruby!"
end

# 파일 끝에 내용 추가하기
File.open("example.txt", "a") do |file|
  file.puts "Adding another line."
end
```
두 스니펫을 실행한 후, `example.txt`의 내용은 다음과 같습니다:
```
Hello, Ruby!
Adding another line.
```

### 제3의 라이브러리 사용하기: FileUtils
더 복잡한 파일 작업을 위해서, Ruby 표준 라이브러리 `FileUtils`가 유용할 수 있으며, 기본 파일 쓰기를 위해서는 표준 `File` 메소드가 충분합니다. 그러나, 파일 쓰기와 함께 복사, 이동, 제거 또는 다른 파일 시스템 작업을 수행하고자 한다면, `FileUtils`를 탐색해볼 만합니다.

디렉토리를 생성한 다음 해당 디렉토리 안에 파일을 쓰기 위해 `FileUtils`를 사용하는 예:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/today.log", "w") do |file|
  file.puts "Log entry: #{Time.now}"
end
```

이것은 이미 존재하지 않는 경우 새로운 디렉토리 `logs`를 생성하고, 그 안에 새 파일 `today.log`에 쓰는 것을 보여주며, 직접적으로 FileUtils로 쓰지 않지만, 디렉토리 처리 능력을 활용하여 디렉토리와 파일 조작을 모두 보여줍니다.
