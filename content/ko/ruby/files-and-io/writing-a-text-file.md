---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:07.828150-07:00
description: "Ruby\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC4F0\uB294\
  \ \uAC83\uC740 \uCD9C\uB825\uACFC \uB370\uC774\uD130\uB97C \uC601\uAD6C\uC801\uC73C\
  \uB85C \uC800\uC7A5\uD560 \uC218 \uC788\uAC8C \uD558\uC5EC, \uB098\uC911\uC5D0 \uC811\
  \uADFC\uD558\uAC70\uB098 \uC218\uC815\uD560 \uC218 \uC788\uAC8C \uD558\uB294 \uAE30\
  \uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uB85C\uAE45, \uC124\uC815 \uC800\uC7A5, \uC0AC\uB78C\uC774 \uC77D\uC744\
  \ \uC218 \uC788\uB294 \uD615\uC2DD\uC73C\uB85C \uB370\uC774\uD130\uB97C \uB0B4\uBCF4\
  \uB0B4\uB294 \uAC83\uACFC \uAC19\uC740 \uC774\uC720\uB85C \uC790\uC8FC \uC774 \uC791\
  \uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:14.935304
model: gpt-4-0125-preview
summary: "Ruby\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC4F0\uB294 \uAC83\
  \uC740 \uCD9C\uB825\uACFC \uB370\uC774\uD130\uB97C \uC601\uAD6C\uC801\uC73C\uB85C\
  \ \uC800\uC7A5\uD560 \uC218 \uC788\uAC8C \uD558\uC5EC, \uB098\uC911\uC5D0 \uC811\
  \uADFC\uD558\uAC70\uB098 \uC218\uC815\uD560 \uC218 \uC788\uAC8C \uD558\uB294 \uAE30\
  \uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uB85C\uAE45, \uC124\uC815 \uC800\uC7A5, \uC0AC\uB78C\uC774 \uC77D\uC744\
  \ \uC218 \uC788\uB294 \uD615\uC2DD\uC73C\uB85C \uB370\uC774\uD130\uB97C \uB0B4\uBCF4\
  \uB0B4\uB294 \uAC83\uACFC \uAC19\uC740 \uC774\uC720\uB85C \uC790\uC8FC \uC774 \uC791\
  \uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇과 왜?
Ruby에서 텍스트 파일을 쓰는 것은 출력과 데이터를 영구적으로 저장할 수 있게 하여, 나중에 접근하거나 수정할 수 있게 하는 기본적인 작업입니다. 프로그래머들은 로깅, 설정 저장, 사람이 읽을 수 있는 형식으로 데이터를 내보내는 것과 같은 이유로 자주 이 작업을 수행합니다.

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
