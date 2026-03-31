# Juego Futoshiki

La descripción completa de la práctica está en el archivo `Practica TLP 2026.pdf`.

## Resumen rápido
Instrucciones para ejecutar archivos Haskell (.hs) tanto dentro del contenedor Docker como de forma local.

## Requisitos
- Docker (para usar el contenedor provisto)
- O bien GHC / GHCi / runghc instalados localmente

## Uso (con Docker)
1. Construir y levantar el contenedor:

```bash
docker compose up --build -d
```

2. Abrir un shell en el servicio Haskell:

```bash
docker compose exec haskell bash
```

3. Comandos útiles dentro del contenedor:

- Ejecutar un archivo sin compilar:

```bash
runghc Main.hs
```

- Abrir `ghci` (interactivo):

```bash
ghci Main.hs
# dentro de ghci: puedes llamar a `main` o a otras funciones (ej. `signo 3`)
```

- Compilar a binario y ejecutar:

```bash
ghc -o main Main.hs
./main
```

4. Ejecutar un archivo sin entrar al shell (comando directo):

```bash
docker compose run --rm haskell runghc Main.hs
```

o, si el contenedor ya está en ejecución:

```bash
docker compose exec haskell runghc Main.hs
```

## Uso local (Windows)
Si tienes GHC instalado localmente:

```powershell
runghc Main.hs
ghci Main.hs
ghc -o main.exe main.hs
.\main.exe
```

