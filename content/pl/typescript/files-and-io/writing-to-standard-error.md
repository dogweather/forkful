---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:47.558706-07:00
description: "Jak to zrobi\u0107: TypeScript, b\u0119d\u0105c nadzbiorem JavaScript,\
  \ opiera si\u0119 na bazowym \u015Brodowisku wykonawczym JS (jak Node.js) do pisania\
  \ na stderr. Oto jak mo\u017Cna\u2026"
lastmod: '2024-03-13T22:44:35.156145-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, b\u0119d\u0105c nadzbiorem JavaScript, opiera si\u0119 na bazowym\
  \ \u015Brodowisku wykonawczym JS (jak Node.js) do pisania na stderr."
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić:
TypeScript, będąc nadzbiorem JavaScript, opiera się na bazowym środowisku wykonawczym JS (jak Node.js) do pisania na stderr. Oto jak można to zrobić bezpośrednio:

```typescript
console.error("To jest komunikat błędu.");
```

Przykładowe wyjście na stderr:
```
To jest komunikat błędu.
```

W środowisku Node.js można również użyć metody `process.stderr.write()` do pisania na niższym poziomie:

```typescript
process.stderr.write("Komunikat błędu niskiego poziomu.\n");
```

Przykładowe wyjście na stderr:
```
Komunikat błędu niskiego poziomu.
```

Dla bardziej strukturalnego rejestrowania błędów, można używać popularnych bibliotek stron trzecich, takich jak `winston` lub `pino`. Oto jak rejestrować błędy używając `winston`:

Najpierw zainstaluj `winston`:

```bash
npm install winston
```

Następnie użyj go w swoim pliku TypeScript:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Błąd zarejestrowany za pomocą winston.');
```

To zapisze błąd zarówno do konsoli, jak i do pliku o nazwie `error.log`. Pamiętaj, że przy zapisywaniu do plików ważne jest zarządzanie uprawnieniami do plików i ich rotacją, aby zapobiec problemom związanym z użyciem miejsca na dysku.
