---
title:                "두 날짜 비교하기"
html_title:           "Bash: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 자 뭐여 이거?
비교는 뭐냐면 두 날짜를 비교하는 것입니다. 왜 프로그래머들이 이런 걸 하는가 묻는다면, 데이터베이스나 파일 시스템 속의 날짜를 정렬하거나 원하는 시간 순서대로 파일들을 정리하기 위해서입니다.

## 어떻게 하지?
```Bash
$ date1="2020-05-10"
$ date2="2020-05-11"

$ if [[ $date1 > $date2 ]]; then
>     echo "$date1 is later than $date2"
> else
>     echo "$date2 is later than $date1"
> fi
2020-05-11 is later than 2020-05-10
```

## 깊게 파고들어보자
- 역사적 배경: 날짜 비교는 Unix 시스템에서도 사용되었던 방법이었습니다. Unix의 파일 시스템에서는 파일의 생성 시간을 관리하기 위해 날짜 비교를 사용하였습니다.
- 대안: 비록 Bash에서도 날짜 비교가 가능하지만, 다른 프로그래밍 언어들은 각자의 방식으로 날짜 비교를 구현할 수 있습니다. 예를 들어, Python에서는 datetime 라이브러리를 이용해 날짜 비교를 할 수 있습니다.
- 구현 세부사항: Bash에서는 비교 연산자인 `>` 또는 `<`을 이용해 날짜를 비교할 수 있습니다. 또한 `if`와 `fi`를 이용하여 조건문을 만들어 날짜 비교 결과에 따라 다른 명령을 실행할 수 있습니다.

## 참고 자료
- [Unix 시스템에서의 날짜 관리 방식](https://medium.com/@leighhalliday/how-to-compare-two-dates-in-a-bash-shell-script-2d7df171972e)
- [Python의 datetime 라이브러리](https://docs.python.org/3/library/datetime.html)