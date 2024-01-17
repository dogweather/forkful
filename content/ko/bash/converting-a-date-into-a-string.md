---
title:                "날짜를 문자열로 변환하기"
html_title:           "Bash: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 문자열로 변환하는 것은 프로그래머들이 날짜 데이터를 다루는데 더 편리하게 하기 위함입니다. 간단한 형식의 문자열로 변환된 날짜를 사용하면, 예를 들어서 파일 이름을 지정하거나 데이터베이스에서 검색할 때 유용하게 사용할 수 있습니다.

## 방법:
```Bash
# 현재 날짜를 문자열로 변환
date=$(date +'%Y-%m-%d')
echo $date

# 특정 날짜를 지정한 형식으로 변환
date=$(date -d '20210101' +'%d/%m/%Y')
echo $date
```

출력:
```
2021-08-16
01/01/2021
```

## 깊이 파고들기:
날짜를 문자열로 변환하는 개념은 실제로 1970년대 UNIX 시스템에서 처음으로 도입되었습니다. 이렇게 하는 것은 날짜를 표현하는 다양한 방법 중에서 가장 효율적인 방식입니다. 또 다른 대안으로는 ISO 8601 형식을 사용하는 것인데, 이것은 숫자로 표현된 날짜를 의미합니다. 날짜를 문자열로 변환하는 방법은 ```date``` 명령어를 사용해서 구현할 수 있습니다. 보다 복잡한 형식의 날짜를 변환하기 위해서는 더 많은 옵션을 사용할 수 있습니다.

## 참고 자료:
- [GNU Coreutils - date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)