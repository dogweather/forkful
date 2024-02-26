---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:23.528480-07:00
description: "\uB8E8\uBE44\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC\
  \ \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD504\uB85C\uADF8\uB798\
  \uBA38\uAC00 \uD30C\uC77C\uC744 \uC77D\uAC70\uB098 \uC0C8 \uB514\uB809\uD1A0\uB9AC\
  \uB97C \uC0DD\uC131\uD558\uAE30 \uC804\uC5D0 \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\
  \uC7AC\uB97C \uAC80\uC99D\uD560 \uC218 \uC788\uB3C4\uB85D \uD574\uC90D\uB2C8\uB2E4\
  . \uC774\uB294 \uD30C\uC77C \uCC98\uB9AC \uC2DC \uC624\uB958\uB97C \uD53C\uD558\uACE0\
  \ \uD30C\uC77C \uC2DC\uC2A4\uD15C \uC870\uC791\uC758 \uC2E0\uB8B0\uC131\uC744 \uBCF4\
  \uC7A5\uD558\uB294 \uB370 \uC911\uC694\uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.995878-07:00'
model: gpt-4-0125-preview
summary: "\uB8E8\uBE44\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\
  \uBD80\uB97C \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD504\uB85C\uADF8\uB798\uBA38\
  \uAC00 \uD30C\uC77C\uC744 \uC77D\uAC70\uB098 \uC0C8 \uB514\uB809\uD1A0\uB9AC\uB97C\
  \ \uC0DD\uC131\uD558\uAE30 \uC804\uC5D0 \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC\
  \uB97C \uAC80\uC99D\uD560 \uC218 \uC788\uB3C4\uB85D \uD574\uC90D\uB2C8\uB2E4. \uC774\
  \uB294 \uD30C\uC77C \uCC98\uB9AC \uC2DC \uC624\uB958\uB97C \uD53C\uD558\uACE0 \uD30C\
  \uC77C \uC2DC\uC2A4\uD15C \uC870\uC791\uC758 \uC2E0\uB8B0\uC131\uC744 \uBCF4\uC7A5\
  \uD558\uB294 \uB370 \uC911\uC694\uD569\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
루비에서 디렉토리의 존재 여부를 확인하는 것은 프로그래머가 파일을 읽거나 새 디렉토리를 생성하기 전에 디렉토리의 존재를 검증할 수 있도록 해줍니다. 이는 파일 처리 시 오류를 피하고 파일 시스템 조작의 신뢰성을 보장하는 데 중요합니다.

## 방법:
루비의 표준 라이브러리는 디렉토리의 존재 여부를 확인하기 위한 직관적인 메소드를 제공합니다. 여기 제3자 라이브러리가 필요 없는 순수 루비를 사용한 방법이 있습니다:

```ruby
require 'fileutils'

# 디렉토리가 존재하는지 확인
if Dir.exist?('/path/to/directory')
  puts '디렉토리가 존재합니다.'
else
  puts '디렉토리가 존재하지 않습니다.'
end
```
샘플 출력:
```
디렉토리가 존재합니다.
```
또는:
```
디렉토리가 존재하지 않습니다.
```

`Dir.exist?`를 사용하는 것 외에도 주어진 경로가 디렉토리인 경우 `true`를 반환하는 `File.directory?` 메소드를 활용할 수도 있습니다:

```ruby
if File.directory?('/path/to/directory')
  puts '디렉토리가 존재합니다.'
else
  puts '디렉토리가 존재하지 않습니다.'
end
```
`Dir.exist?` 및 `File.directory?`는 루비의 표준 라이브러리의 일부이며 외부 젬을 사용하지 않아도 되므로 디렉토리 확인을 위한 편리하고 효율적인 옵션입니다.
