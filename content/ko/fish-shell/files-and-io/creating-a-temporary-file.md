---
date: 2024-01-20 17:40:13.226799-07:00
description: "How to: (\uC0AC\uC6A9 \uBC29\uBC95:) Fish Shell\uC5D0\uC11C \uC784\uC2DC\
  \ \uD30C\uC77C\uC744 \uB9CC\uB4E4\uACE0 \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95\uC785\
  \uB2C8\uB2E4. \uC9C1\uAD00\uC801\uC774\uACE0 \uAC04\uB2E8\uD558\uC8E0."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.467480-06:00'
model: gpt-4-1106-preview
summary: "(\uC0AC\uC6A9 \uBC29\uBC95:) Fish Shell\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\
  \uC744 \uB9CC\uB4E4\uACE0 \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4\
  ."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

## How to: (사용 방법:)
Fish Shell에서 임시 파일을 만들고 사용하는 방법입니다. 직관적이고 간단하죠.

```Fish Shell
# 임시 파일 생성
set tempfile (mktemp)

# 임시 파일에 데이터 쓰기
echo "임시 데이터" > $tempfile

# 임시 파일 내용 확인
cat $tempfile

# 임시 파일 삭제
rm $tempfile
```

위 코드 실행 결과, 임시 파일에 '임시 데이터'가 저장되었음을 확인할 수 있고, 마지막에 파일을 삭제합니다.

## Deep Dive (심층 분석)
UNIX 시스템에서는 전통적으로 `/tmp` 폴더가 임시 파일 저장을 위해 사용되어 왔습니다. `mktemp` 명령은 이런 관습을 따르며 안전한 임시 파일 이름을 생성합니다. Fish Shell은 이러한 유닉스 명령들과 잘 통합되어 사용하기 쉽습니다. `mktemp`의 대안으로는 직접 파일 경로를 지정하여 touch 명령을 사용할 수 있지만, 이는 충돌 가능성을 열어두므로 추천되지 않습니다. 안전하게 임시 파일을 생성하여 사용할 때는 `mktemp`를 활용하면 됩니다.

## See Also (참고 자료)
- [Fish Documentation](https://fishshell.com/docs/current/index.html)
- [mktemp man page](https://man7.org/linux/man-pages/man1/mktemp.1.html)
- [UNIX /tmp directory](https://en.wikipedia.org/wiki/Temporary_folder)
