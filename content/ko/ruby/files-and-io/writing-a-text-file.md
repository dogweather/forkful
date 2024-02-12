---
title:                "텍스트 파일 쓰기"
aliases:
- /ko/ruby/writing-a-text-file/
date:                  2024-02-03T19:29:07.828150-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 파일 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
