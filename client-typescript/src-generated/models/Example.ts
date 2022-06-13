/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExampleV1 } from './ExampleV1';
import type { ExampleV2 } from './ExampleV2';
import type { ExampleV3 } from './ExampleV3';

/**
 * The format comes in multiple distinct versions
 */
export type Example = (ExampleV1 | ExampleV2 | ExampleV3);

