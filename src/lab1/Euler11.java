package lab1;

import java.util.List;

public class Euler11 {

    public static long solve(List<List<Integer>> grid, int seqLen) {
        int height = grid.size();
        if (height == 0) return 0L;
        int width = grid.get(0).size();
        long maxProduct = 0L;

        // Горизонтальные
        if (width >= seqLen) {
            for (int i = 0; i < height; i++) {
                for (int j = 0; j <= width - seqLen; j++) {
                    long product = 1L;
                    for (int k = 0; k < seqLen; k++) {
                        product *= grid.get(i).get(j + k);
                    }
                    if (product > maxProduct) maxProduct = product;
                }
            }
        }

        // Вертикальные
        if (height >= seqLen) {
            for (int i = 0; i <= height - seqLen; i++) {
                for (int j = 0; j < width; j++) {
                    long product = 1L;
                    for (int k = 0; k < seqLen; k++) {
                        product *= grid.get(i + k).get(j);
                    }
                    if (product > maxProduct) maxProduct = product;
                }
            }
        }

        // Диагональные (вниз-вправо)
        if (width >= seqLen && height >= seqLen) {
            for (int i = 0; i <= height - seqLen; i++) {
                for (int j = 0; j <= width - seqLen; j++) {
                    long product = 1L;
                    for (int k = 0; k < seqLen; k++) {
                        product *= grid.get(i + k).get(j + k);
                    }
                    if (product > maxProduct) maxProduct = product;
                }
            }
        }

        // Диагональные (вниз-влево)
        if (width >= seqLen && height >= seqLen) {
            for (int i = 0; i <= height - seqLen; i++) {
                for (int j = seqLen - 1; j < width; j++) {
                    long product = 1L;
                    for (int k = 0; k < seqLen; k++) {
                        product *= grid.get(i + k).get(j - k);
                    }
                    if (product > maxProduct) maxProduct = product;
                }
            }
        }

        return maxProduct;
    }
}