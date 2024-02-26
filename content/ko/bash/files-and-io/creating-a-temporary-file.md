---
date: 2024-01-20 17:39:59.526794-07:00
description: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uC740 \uBB34\uC5C7\uC77C\uAE4C\
  \uC694? \uD504\uB85C\uADF8\uB7A8\uC774 \uB370\uC774\uD130\uB97C \uC784\uC2DC\uC801\
  \uC73C\uB85C \uCC98\uB9AC\uD558\uAC70\uB098 \uC800\uC7A5\uD558\uAE30 \uC704\uD574\
  \ \uC0AC\uC6A9\uB418\uB294 \uD30C\uC77C\uC785\uB2C8\uB2E4. \uC65C \uD544\uC694\uD560\
  \uAE4C\uC694? \uD504\uB85C\uADF8\uB7A8 \uC791\uB3D9 \uC911 \uB0A8\uAE30\uACE0 \uC2F6\
  \uC9C0 \uC54A\uC740 \uB370\uC774\uD130\uB97C \uB2E4\uB8E8\uAC70\uB098, \uCDA9\uB3CC\
  \ \uBC29\uC9C0\uC640 \uB370\uC774\uD130 \uBCF4\uD638\uB97C \uC704\uD574\uC11C\uC8E0\
  ."
isCJKLanguage: true
lastmod: '2024-02-25T18:49:52.502125-07:00'
model: gpt-4-1106-preview
summary: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uC740 \uBB34\uC5C7\uC77C\uAE4C\uC694\
  ? \uD504\uB85C\uADF8\uB7A8\uC774 \uB370\uC774\uD130\uB97C \uC784\uC2DC\uC801\uC73C\
  \uB85C \uCC98\uB9AC\uD558\uAC70\uB098 \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC0AC\
  \uC6A9\uB418\uB294 \uD30C\uC77C\uC785\uB2C8\uB2E4. \uC65C \uD544\uC694\uD560\uAE4C\
  \uC694? \uD504\uB85C\uADF8\uB7A8 \uC791\uB3D9 \uC911 \uB0A8\uAE30\uACE0 \uC2F6\uC9C0\
  \ \uC54A\uC740 \uB370\uC774\uD130\uB97C \uB2E4\uB8E8\uAC70\uB098, \uCDA9\uB3CC \uBC29\
  \uC9C0\uC640 \uB370\uC774\uD130 \uBCF4\uD638\uB97C \uC704\uD574\uC11C\uC8E0."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
임시 파일 생성은 무엇일까요? 프로그램이 데이터를 임시적으로 처리하거나 저장하기 위해 사용되는 파일입니다. 왜 필요할까요? 프로그램 작동 중 남기고 싶지 않은 데이터를 다루거나, 충돌 방지와 데이터 보호를 위해서죠.

## 사용법:
임시 파일은 간단하게 만들 수 있습니다. 여기 Bash에서 임시 파일을 만들고 사용한 다음 삭제하는 방법에 대한 명령어와 예시가 있습니다.

```Bash
# 임시 파일을 생성하고 파일명을 얻기
temp_file=$(mktemp)
echo "임시 파일이 생성되었습니다: $temp_file"

# 임시 파일 사용
echo "임시 데이터" > "$temp_file"

# 임시 파일의 내용 확인
cat "$temp_file"

# 임시 파일 삭제
rm "$temp_file"
echo "임시 파일이 삭제되었습니다."
```

## 심층 분석:
임시 파일은 Unix 시스템이 시작된 이래로 몇십 년간 사용되어 왔습니다. 초기 버전의 Unix에서는 `mktemp` 명령어가 없었고, 임시 파일을 생성할 때는 보통 파일명에 시간이나 프로세스 ID를 추가해서 사용했습니다. 하지만 이런 방법은 안전하지 않아 `mktemp` 유틸리티가 나오게 되었습니다.

대체 방법으로, Bash의 임시 파일 생성 없이 파일 디스크립터를 통해 데이터를 전달하는 방법도 있습니다.

```Bash
# 임시 파일 디스크립터 생성
exec 3<>$(mktemp)

# 임시 파일에 데이터 쓰기
echo "임시 데이터" >&3

# 임시 파일의 데이터 읽기
cat <&3

# 파일 디스크립터 닫기
exec 3>&-
```

이 방법은 파일명을 사용하지 않기 때문에 더 안전하고 효율적일 수 있습니다. 하지만 파일 디스크립터를 관리하는 것이 복잡할 수 있으므로 상황에 맞게 사용해야 합니다.

## 참고 자료:
- Bash Reference Manual: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- mktemp Man Page: https://man7.org/linux/man-pages/man1/mktemp.1.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
