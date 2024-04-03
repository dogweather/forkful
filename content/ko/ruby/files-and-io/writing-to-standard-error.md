---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:41.767548-07:00
description: "\uBC29\uBC95: \uB8E8\uBE44\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB294 `$stderr` \uB610\uB294 `STDERR`\uC744 \uC0AC\uC6A9\uD558\uC5EC stderr\uC5D0\
  \ \uC4F0\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4\
  . \uC774 \uAE30\uBCF8 \uC791\uC5C5\uC744 \uC704\uD574 \uD0C0\uC0AC \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uAC00 \uD544\uC694\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. #."
lastmod: '2024-03-13T22:44:56.019918-06:00'
model: gpt-4-0125-preview
summary: "\uB8E8\uBE44\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 `$stderr`\
  \ \uB610\uB294 `STDERR`\uC744 \uC0AC\uC6A9\uD558\uC5EC stderr\uC5D0 \uC4F0\uB294\
  \ \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
weight: 25
---

## 방법:
루비의 표준 라이브러리는 `$stderr` 또는 `STDERR`을 사용하여 stderr에 쓰는 간단한 방법을 제공합니다. 이 기본 작업을 위해 타사 라이브러리가 필요하지 않습니다.

### stderr에 간단한 메시지 쓰기:
```ruby
$stderr.puts "Error: 파일을 찾을 수 없습니다."
# 또는 동등하게
STDERR.puts "Error: 파일을 찾을 수 없습니다."
```
stderr에 대한 샘플 출력:
```
Error: 파일을 찾을 수 없습니다.
```

### stderr를 파일로 리다이렉션:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "구성을 열지 못했습니다."
end
```
이 코드 조각은 stderr를 `error.log`라는 파일로 리다이렉션하고, 그 후부터 프로그램이 stderr 리다이렉션을 재설정하거나 종료될 때까지 모든 후속 쓰기 오류가 그곳에 출력됩니다.

### 예외 처리를 사용한 stderr:
```ruby
begin
  # 실패할 수 있는 작업 시뮬레이션, 예를 들어, 파일 열기
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "예외 발생: #{e.message}"
end
```
stderr에 대한 샘플 출력:
```
예외 발생: No such file or directory @ rb_sysopen - nonexistent_file.txt
```

루비의 내장 메서드로 많은 애플리케이션에서 stderr에 쓰기에 충분하지만, 보다 복잡한 로깅 요구 사항의 경우 `logger` 표준 라이브러리나 `Log4r`과 같은 외부 젬을 고려할 수 있습니다. 이러한 것들은 중요도 수준, 서식, 파일, 이메일 등 다양한 출력에 쓸 수 있는 기능을 포함한 구성 가능한 로깅 메커니즘을 제공합니다.
