# Simball Player API (Common Lisp)

En REST-tjänst i Common Lisp (Clack) som implementerar Simball Player API enligt swagger.json.

## Starta tjänsten

1. Installera Quicklisp och Common Lisp (t.ex. SBCL).
2. Installera beroenden:
   ```lisp
   (ql:quickload :simball-player)
   ```
3. Starta servern:
   ```lisp
   (simball-player:start-server)
   ```

## API-endpoints

Alla endpoints prefixas med strategi, t.ex. `/default/Player`.

- `GET /:strategy/Player` – Returnerar namn/status.
- `GET /:strategy/Player/setup` – Returnerar laguppställning.
- `POST /:strategy/Player/update` – Tar emot matchstatus och returnerar instruktioner.

## Lägg till strategier

Lägg till strategi-namn i `*strategies*` i `src/main.lisp` och implementera logik i POST `/update`.

## Quicklisp: Lägg till projektet som lokal källa

Om du får problem med att ladda systemet, lägg till projektmappen som lokal källa i Quicklisp:

```lisp
(ql:add-to-local-projects #P"/workspaces/simball-player/")
```

Därefter kan du ladda systemet som vanligt:

```lisp
(ql:quickload :simball-player)
```