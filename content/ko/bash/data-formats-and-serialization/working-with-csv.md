---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:51.431508-07:00
description: "Bash\uC5D0\uC11C CSV(\uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12) \uD30C\
  \uC77C\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 \uD3C9\uBB38 \uD615\uC2DD\uC73C\uB85C\
  \ \uC800\uC7A5\uB41C \uD45C \uD615\uC2DD \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD558\
  \uACE0 \uC870\uC791\uD558\uB294 \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\
  \uB2E4. \uC774\uB294 \uB370\uC774\uD130 \uBCC0\uD658, \uBD84\uC11D, \uADF8\uB9AC\
  \uACE0 \uBA85\uB839 \uC904\uC5D0\uC11C \uC9C1\uC811 \uB354 \uBB34\uAC70\uC6B4 \uB3C4\
  \uAD6C\uB098 \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD \uC5C6\uC774 \uC791\uC5C5\
  \uC744 \uC790\uB3D9\uD654\uD560 \uC218 \uC788\uAE30 \uB54C\uBB38\uC5D0 \uD504\uB85C\
  \uADF8\uB798\uBA38\uC5D0\uAC8C\u2026"
lastmod: 2024-02-19 22:05:14.428005
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uC11C CSV(\uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12) \uD30C\
  \uC77C\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 \uD3C9\uBB38 \uD615\uC2DD\uC73C\uB85C\
  \ \uC800\uC7A5\uB41C \uD45C \uD615\uC2DD \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD558\
  \uACE0 \uC870\uC791\uD558\uB294 \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\
  \uB2E4. \uC774\uB294 \uB370\uC774\uD130 \uBCC0\uD658, \uBD84\uC11D, \uADF8\uB9AC\
  \uACE0 \uBA85\uB839 \uC904\uC5D0\uC11C \uC9C1\uC811 \uB354 \uBB34\uAC70\uC6B4 \uB3C4\
  \uAD6C\uB098 \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD \uC5C6\uC774 \uC791\uC5C5\
  \uC744 \uC790\uB3D9\uD654\uD560 \uC218 \uC788\uAE30 \uB54C\uBB38\uC5D0 \uD504\uB85C\
  \uADF8\uB798\uBA38\uC5D0\uAC8C\u2026"
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Bash에서 CSV(쉼표로 구분된 값) 파일을 다루는 것은 평문 형식으로 저장된 표 형식 데이터를 처리하고 조작하는 것과 관련이 있습니다. 이는 데이터 변환, 분석, 그리고 명령 줄에서 직접 더 무거운 도구나 프로그래밍 환경 없이 작업을 자동화할 수 있기 때문에 프로그래머에게 필수적입니다.

## 방법:

**CSV 파일을 줄 단위로 읽기**

```bash
while IFS=, read -r 컬럼1 컬럼2 컬럼3
do
  echo "컬럼 1: $컬럼1, 컬럼 2: $컬럼2, 컬럼 3: $컬럼3"
done < 예시.csv
```

*샘플 출력:*

```
컬럼 1: id, 컬럼 2: 이름, 컬럼 3: 이메일
...
```

**조건에 따라 CSV 행 필터링하기**

`awk`를 사용하면 쉽게 행을 필터링할 수 있습니다. 예를 들어, 두 번째 컬럼이 "Alice"인 행을 찾으려면:

```bash
awk -F, '$2 == "Alice" { print $0 }' 예시.csv
```

**컬럼 값 수정하기**

두 번째 컬럼을 대문자로 변경하려면:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' 예시.csv
```

**컬럼을 기준으로 CSV 파일 정렬하기**

예를 들어, 세 번째 컬럼을 기준으로 (숫자로) CSV 파일을 정렬할 수 있습니다:

```bash
sort -t, -k3,3n 예시.csv
```

**더 복잡한 작업을 위한 `csvkit` 사용하기**

`csvkit`은 CSV로 변환하고 작업하기 위한 커맨드 라인 도구 모음입니다. pip를 통해 설치할 수 있습니다.

JSON 파일을 CSV로 변환하기:

```bash
in2csv 데이터.json > 데이터.csv
```

SQL을 사용해 CSV 파일 쿼리하기:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" 예시.csv
```

*참고: `csvkit`을 설치하려면 Python이 필요하며 `pip install csvkit`을 사용하여 할 수 있습니다.*
