---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

category:             "Ruby"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
표준 에러는 프로그램이 실행 중에 발생한 에러 메시지를 출력하는 특별한 데이터 스트림입니다. 프로그래머들은 디버깅을 돕고, 에러와 일반 출력을 분리하기 위해 이를 사용합니다.

## How to: (사용 방법)
Ruby에서 표준 에러로 작성하는 것은 간단합니다. `STDERR.puts` 나 `$stderr.puts`를 사용하세요.

```Ruby
# 표준 에러로 메시지 출력하기
STDERR.puts "오류 발생!"
$stderr.puts "또 오류 발생!"

# 출력 예시:
# 오류 발생!
# 또 오류 발생!
```

프로그램의 일반 출력과 분리하여 에러 메시지를 콘솔에 표시하게 됩니다.

## Deep Dive (심화 탐구)
표준 에러 스트림은 UNIX 시스템의 초기부터 있었으며, 줄 단위 버퍼링을 사용합니다. `STDERR`와 `$stderr`는 둘 다 같은 스트림을 가리키나, `$stderr`는 현재 오류 출력 스트림을 가리키는 글로벌 변수이며 `STDERR`는 기본 스트림의 상수입니다. `$stderr`를 다른 객체로 바꾸어 리디렉션할 수 있습니다.

```Ruby
# $stderr를 재지정하여 에러 메시지를 파일에 기록하기
File.open('error.log', 'w') do |file|
  $stderr = file
  STDERR.puts "오류 로그에 기록됩니다."
end
```

후에 `$stderr`를 다시 `STDERR`로 지정하여 콘솔로 복귀시킬 수 있습니다.

## See Also (참고 자료)
- Ruby 문서의 표준 라이브러리: [IO 클래스](https://ruby-doc.org/core/IO.html)
