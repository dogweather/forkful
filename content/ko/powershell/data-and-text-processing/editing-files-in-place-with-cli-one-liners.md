---
date: 2024-01-27 16:21:09.846719-07:00
description: "PowerShell\uC5D0\uC11C CLI \uC6D0 \uB77C\uC774\uB108\uB85C \uD604\uC7A5\
  \ \uB0B4 \uD30C\uC77C \uD3B8\uC9D1\uC740 \uD3B8\uC9D1\uAE30\uC5D0\uC11C \uD30C\uC77C\
  \uC744 \uC5F4 \uD544\uC694 \uC5C6\uC774 \uBA85\uB839 \uC904\uC5D0\uC11C \uC9C1\uC811\
  \ \uD30C\uC77C\uC744 \uC218\uC815\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\
  \uB2C8\uB2E4. \uC774 \uC811\uADFC \uBC29\uC2DD\uC740 \uC2DC\uAC04\uC744 \uC808\uC57D\
  \uD558\uBA70, \uC5EC\uB7EC \uD30C\uC77C\uC5D0 \uAC78\uCCD0 \uBC18\uBCF5 \uD3B8\uC9D1\
  \ \uC791\uC5C5\uC744 \uC790\uB3D9\uD654\uD558\uAC70\uB098 \uC77C\uAD04 \uCC98\uB9AC\
  \uD558\uAE30\uC5D0 \uD2B9\uD788 \uC720\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:55.540760-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC5D0\uC11C CLI \uC6D0 \uB77C\uC774\uB108\uB85C \uD604\uC7A5\
  \ \uB0B4 \uD30C\uC77C \uD3B8\uC9D1\uC740 \uD3B8\uC9D1\uAE30\uC5D0\uC11C \uD30C\uC77C\
  \uC744 \uC5F4 \uD544\uC694 \uC5C6\uC774 \uBA85\uB839 \uC904\uC5D0\uC11C \uC9C1\uC811\
  \ \uD30C\uC77C\uC744 \uC218\uC815\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\
  \uB2C8\uB2E4."
title: "CLI \uD55C \uC904 \uBA85\uB839\uC5B4\uB85C \uD30C\uC77C\uC744 \uC81C\uC790\
  \uB9AC\uC5D0\uC11C \uD3B8\uC9D1\uD558\uAE30"
weight: 32
---

## 방법:


### 단일 파일에서 텍스트 바꾸기
간단한 작업으로 시작해 보겠습니다: example.txt라는 파일에서 "oldtext"의 모든 인스턴스를 "newtext"로 바꾸려고 합니다. 다음은 그 방법입니다:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

이 원 라이너는 내용을 읽고, 교체를 수행한 다음, 원본 파일에 내용을 다시 씁니다.

### 여러 파일 편집하기
여러 파일에 동일한 변경을 적용해야 하는 경우는 어떨까요? 다음은 루프를 사용하는 접근 방식입니다:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

이 스니펫은 현재 디렉터리의 모든 `.txt` 파일을 찾아 각 파일에서 "oldtext"를 "newtext"로 교체합니다.

### 파일의 시작이나 끝에 내용 추가하기
내용을 추가하거나 앞에 붙이는 작업도 간소화할 수 있습니다:

```PowerShell
# 앞에 붙이기
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# 뒤에 추가하기
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

여기서는 새로운 내용을 기존 내용 앞이나 뒤에 단순히 연결한 다음 되돌려 저장합니다.

## 심층 탐구
역사적으로, 현장 내 편집은 `sed` 및 `awk`와 같은 Unix 도구와 더 일반적으로 연관되어 있습니다. PowerShell은 보다 최근에 등장한 도구로, 기본적으로 전용 현장 내 편집 기능을 포함하고 있지 않습니다. 이는 객체가 텍스트 스트림보다 중요하다는 설계 철학과 부분적으로 관련이 있으며, Unix 도구들이 대부분의 입력을 텍스트로 처리하는 것과는 대조됩니다.

이 작업을 위한 PowerShell 대안으로는 Cygwin 또는 Windows 하위 시스템 for Linux (WSL)을 통해 Windows에서 사용할 수 있는 전통적인 Unix 도구를 사용하는 것입니다. 이러한 도구들은 종종 텍스트 중심 설계 덕분에 현장 내 편집을 위한 더 간결한 문법을 제공합니다.

구현 측면에서는 PowerShell 접근 방식이 전체 파일을 메모리에 읽어들인 후 변경을 가하고 다시 쓴다는 점을 유의해야 합니다. 이는 중간 크기의 파일에는 잘 작동하지만, 매우 큰 파일의 경우 비효율적일 수 있습니다. 이런 경우에는 `.NET` 메서드를 직접 사용하거나 대량의 데이터를 스트리밍하기 위해 설계된 대체 도구를 고려할 수 있습니다.

이러한 고려 사항에도 불구하고, PowerShell의 유연성과 광범위한 기능 세트는 특히 Windows 생태계에 이미 뿌리를 둔 사람들이나 크로스플랫폼 환경을 관리하는 사람들에게 명령 줄에서 직접 파일을 조작하는 데 있어 소중한 도구로 만듭니다.
